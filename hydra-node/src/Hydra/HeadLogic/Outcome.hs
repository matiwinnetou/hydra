{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Outcome where

import Hydra.Prelude

import Hydra.API.ServerOutput (ServerOutput)
import Hydra.Chain (ChainStateType, HeadId, HeadParameters, IsChainState, PostChainTx)
import Hydra.Crypto (MultiSignature, Signature)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (IsTx, TxIdType, UTxOType, ValidationError)
import Hydra.Network.Message (Message)
import Hydra.Party (Party)
import Hydra.Snapshot (Snapshot, SnapshotNumber)

-- | Analogous to events, the pure head logic "core" can have effects emited to
-- the "shell" layers and we distinguish the same: effects onto the client, the
-- network and the chain.
data Effect tx
  = -- | Effect to be handled by the "Hydra.API", results in sending this 'ServerOutput'.
    ClientEffect {serverOutput :: ServerOutput tx}
  | -- | Effect to be handled by a "Hydra.Network", results in a 'Hydra.Network.broadcast'.
    NetworkEffect {message :: Message tx}
  | -- | Effect to be handled by a "Hydra.Chain", results in a 'Hydra.Chain.postTx'.
    OnChainEffect {postChainTx :: PostChainTx tx}
  deriving stock (Generic)

deriving instance (IsChainState tx) => Eq (Effect tx)
deriving instance (IsChainState tx) => Show (Effect tx)
deriving instance (IsChainState tx) => ToJSON (Effect tx)
deriving instance (IsChainState tx) => FromJSON (Effect tx)

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (Effect tx)
  where
  arbitrary = genericArbitrary

-- | Head state changed event. These events represent all the internal state
-- changes, get persisted and processed in an event sourcing manner.
data StateChanged tx
  = HeadInitialized
      { parameters :: HeadParameters
      , chainState :: ChainStateType tx
      , headId :: HeadId
      }
  | CommittedUTxO
      { party :: Party
      , committedUTxO :: UTxOType tx
      , chainState :: ChainStateType tx
      }
  | TransactionAppliedToLocalUTxO
      { tx :: tx
      , utxo :: UTxOType tx
      }
  | SnapshotRequestDecided {snapshotNumber :: SnapshotNumber}
  | SnapshotAppliedToLocalUTxO
      { snapshot :: Snapshot tx
      , txs :: [tx]
      , utxo :: UTxOType tx
      , requestedTxIds :: [TxIdType tx]
      }
  | HeadAborted {chainState :: ChainStateType tx}
  | HeadOpened {chainState :: ChainStateType tx, initialUTxO :: UTxOType tx}
  | HeadClosed {chainState :: ChainStateType tx, contestationDeadline :: UTCTime}
  | HeadFannedOut {chainState :: ChainStateType tx}
  | TransactionReceived {tx :: tx}
  | SnapshotHasBeenConfirmed {snapshot :: Snapshot tx, signatures :: MultiSignature (Snapshot tx)}
  | PartySignedSnapshot {snapshot :: Snapshot tx, sigs :: Map Party (Signature (Snapshot tx))}
  | StateReplaced (HeadState tx)
  deriving stock (Generic)

instance (IsTx tx, Arbitrary (HeadState tx), Arbitrary (ChainStateType tx)) => Arbitrary (StateChanged tx) where
  arbitrary = genericArbitrary

deriving instance (IsTx tx, Eq (HeadState tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving instance (IsTx tx, Show (HeadState tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)
deriving instance (IsTx tx, FromJSON (HeadState tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx)

data Outcome tx
  = NoOutcome
  | Effects {effects :: [Effect tx]}
  | StateChanged (StateChanged tx)
  | Wait {reason :: WaitReason tx}
  | Error {error :: LogicError tx}
  | Combined {left :: Outcome tx, right :: Outcome tx}
  deriving stock (Generic)

deriving instance (IsChainState tx) => Eq (Outcome tx)
deriving instance (IsChainState tx) => Show (Outcome tx)
deriving instance (IsChainState tx) => ToJSON (Outcome tx)
deriving instance (IsChainState tx) => FromJSON (Outcome tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (Outcome tx) where
  arbitrary = genericArbitrary

collectEffects :: Outcome tx -> [Effect tx]
collectEffects = \case
  NoOutcome -> []
  Error _ -> []
  Wait _ -> []
  StateChanged _ -> []
  Effects effs -> effs
  Combined l r -> collectEffects l <> collectEffects r

collectWaits :: Outcome tx -> [WaitReason tx]
collectWaits = \case
  NoOutcome -> []
  Error _ -> []
  Wait w -> [w]
  StateChanged _ -> []
  Effects _ -> []
  Combined l r -> collectWaits l <> collectWaits r

data WaitReason tx
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingFor :: SnapshotNumber}
  | WaitOnSeenSnapshot
  | WaitOnTxs {waitingForTxIds :: [TxIdType tx]}
  | WaitOnContestationDeadline
  deriving stock (Generic)

deriving instance (IsTx tx) => Eq (WaitReason tx)
deriving instance (IsTx tx) => Show (WaitReason tx)
deriving instance (IsTx tx) => ToJSON (WaitReason tx)
deriving instance (IsTx tx) => FromJSON (WaitReason tx)

instance IsTx tx => Arbitrary (WaitReason tx) where
  arbitrary = genericArbitrary
