{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server (
  Server (..),
  ServerCallback,
  ServerComponent,
  withAPIServer,
  APIServerLog,
) where

import Hydra.Prelude hiding (TVar, readTVar, seq)

import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (IOException)
import qualified Data.Aeson as Aeson
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..), convertTxToCBOR)
import Hydra.Chain (IsChainState)
import Hydra.Ledger (IsTx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, NodeId (..), PortNumber)
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (..))
import Network.WebSockets (
  PendingConnection (pendingRequest),
  RequestHead (..),
  acceptRequest,
  receiveData,
  runServer,
  sendTextData,
  sendTextDatas,
  withPingThread,
 )
import Test.QuickCheck (oneof)
import Text.URI

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary APIServerLog where
  arbitrary =
    oneof
      [ APIServerStarted <$> arbitrary
      , pure NewAPIConnection
      , pure $ APIOutputSent (Aeson.Object mempty)
      , pure $ APIInputReceived (Aeson.Object mempty)
      , APIInvalidInput <$> arbitrary <*> arbitrary
      ]

-- | Handle to provide a means for sending server outputs to clients.
newtype Server tx m = Server
  { -- | Send some output to all connected clients.
    sendOutput :: ServerOutput tx -> m ()
  }

-- | Callback for receiving client inputs.
type ServerCallback tx m = ClientInput tx -> m ()

-- | A type tying both receiving input and sending output into a /Component/.
type ServerComponent tx m a = ServerCallback tx m -> (Server tx m -> m a) -> m a

withAPIServer ::
  forall tx.
  (IsChainState tx) =>
  IP ->
  PortNumber ->
  Party ->
  PersistenceIncremental (TimedServerOutput tx) IO ->
  Tracer IO APIServerLog ->
  ServerComponent tx IO ()
withAPIServer host port party PersistenceIncremental{loadAll, append} tracer callback action = do
  responseChannel <- newBroadcastTChanIO
  h <- loadAll
  -- NOTE: we need to reverse the list because we store history in a reversed
  -- list in memory but in order on disk
  history <- newTVarIO (reverse h)
  now <- getCurrentTime
  serverStart <- newTVarIO now

  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  void $ appendToHistory history (Greetings party)

  race_
    (runAPIServer host port tracer history serverStart callback responseChannel)
    . action
    $ Server
      { sendOutput = \output -> do
          timedOutput <- appendToHistory history output
          atomically $
            writeTChan responseChannel timedOutput
      }
 where
  appendToHistory :: TVar [TimedServerOutput tx] -> ServerOutput tx -> IO (TimedServerOutput tx)
  appendToHistory history output = do
    time <- getCurrentTime
    timedOutput <- atomically $ do
      seq <- nextSequenceNumber <$> readTVar history
      let timedOutput = TimedServerOutput{output, time, seq}
      modifyTVar' history (timedOutput :)
      pure timedOutput
    append timedOutput
    pure timedOutput

nextSequenceNumber :: [TimedServerOutput tx] -> Natural
nextSequenceNumber historyList =
  case historyList of
    [] -> 0
    (TimedServerOutput{seq} : _) -> seq + 1

runAPIServer ::
  forall tx.
  (IsChainState tx) =>
  IP ->
  PortNumber ->
  Tracer IO APIServerLog ->
  TVar [TimedServerOutput tx] ->
  TVar UTCTime ->
  (ClientInput tx -> IO ()) ->
  TChan (TimedServerOutput tx) ->
  IO ()
runAPIServer host port tracer history serverStart callback responseChannel = do
  traceWith tracer (APIServerStarted port)
  handle onIOException $
    runServer (show host) (fromIntegral port) $ \pending -> do
      let path = requestPath $ pendingRequest pending
      queryParams <- uriQuery <$> mkURIBs path
      -- serveTxCBOR <- shouldDisplayTxCBOR queryParams
      con <- acceptRequest pending
      chan <- STM.atomically $ dupTChan responseChannel
      traceWith tracer NewAPIConnection

      dontServeHistory <- shouldNotServeHistory queryParams
      if dontServeHistory
        then do
          now <- atomically $ readTVar serverStart
          forwardHistory con (filter (\TimedServerOutput{time} -> time > now))
        else forwardHistory con (filter (const True))

      withPingThread con 30 (pure ()) $
        race_ (receiveInputs con) (sendOutputs chan con)
 where
  -- we want to serve the history unless client specifically asks us not to
  shouldNotServeHistory qp = do
    k <- mkQueryKey "history"
    v <- mkQueryValue "0"
    pure $ (QueryParam k v) `elem` qp

  shouldDisplayTxCBOR qp = do
    k <- mkQueryKey "tx-output"
    v <- mkQueryValue "cbor"
    pure $ (QueryParam k v) `elem` qp

  onIOException ioException =
    throwIO $
      RunServerException
        { ioException
        , host
        , port
        }

  sendOutputs chan con = forever $ do
    response <- STM.atomically $ readTChan chan
    traceShowM "response"
    traceShowM response
    -- let encodedResponse = Aeson.encode response
    let sentResponse = Aeson.encode response
    -- let sentResponse =
    --       bool encodedResponse (convertTxToCBOR response) txAsCbor
    sendTextData con sentResponse
    traceWith tracer (APIOutputSent $ toJSON response)

  receiveInputs con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right input -> do
        traceWith tracer (APIInputReceived $ toJSON input)
        callback input
      Left e -> do
        -- XXX(AB): toStrict might be problematic as it implies consuming the full
        -- message to memory
        let clientInput = decodeUtf8With lenientDecode $ toStrict msg
        time <- getCurrentTime
        seq <- atomically $ nextSequenceNumber <$> readTVar history
        let timedOutput = TimedServerOutput{output = InvalidInput @tx e clientInput, time, seq}
        sendTextData con $ Aeson.encode timedOutput
        traceWith tracer (APIInvalidInput e clientInput)

  forwardHistory con predicate = do
    hist' <- STM.atomically (readTVar history)
    let hist = predicate hist'
    let encodeAndReverse xs serverOutput = Aeson.encode serverOutput : xs
    sendTextDatas con $ foldl' encodeAndReverse [] hist

data RunServerException = RunServerException
  { ioException :: IOException
  , host :: IP
  , port :: PortNumber
  }
  deriving (Show)

instance Exception RunServerException
