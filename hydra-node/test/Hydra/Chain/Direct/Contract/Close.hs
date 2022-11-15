{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize')
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), addParticipationTokens, changeHeadOutputDatum, genHash)
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId, testPolicyId)
import Hydra.Chain.Direct.Tx (ClosingSnapshot (..), OpenThreadOutput (..), UTxOHash (UTxOHash), closeTx, mkHeadOutput)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genPointInTimeWithSlotDifference, slotNoToUTCTime)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber, genSnapShot)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (arbitrarySizedNatural, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

--
-- CloseTx
--

healthyCloseTx :: (Tx, UTxO)
healthyCloseTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      somePartyCardanoVerificationKey
      healthyClosingSnapshot
      pointInTime
      startSlot
      openThreadOutput

  headInput = generateWith arbitrary 42
  -- here we need to pass in contestation period when generating start/end tx validity slots/time
  -- since if tx validity bound difference is bigger than contestation period our close validator
  -- will fail
  (startSlot, pointInTime) =
    genPointInTimeWithSlotDifference (fromIntegral genContestationPeriodSeconds) `generateWith` 42
  headResolvedInput =
    mkHeadOutput testNetworkId testPolicyId headTxOutDatum
      & addParticipationTokens healthyParties

  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyCloseDatum)

  headDatum = fromPlutusData $ toData healthyCloseDatum

  lookupUTxO = UTxO.singleton (headInput, headResolvedInput)

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (headInput, headResolvedInput, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

healthyCloseUpperBoundSlotNo :: SlotNo
healthyCloseUpperBoundSlotNo = arbitrary `generateWith` 42

healthyClosingSnapshot :: ClosingSnapshot
healthyClosingSnapshot =
  CloseWithConfirmedSnapshot
    { snapshotNumber = genSnapshotNumber
    , closeUtxoHash = UTxOHash $ hashUTxO @Tx healthyCloseUTxO
    , signatures = healthySignature genSnapshotNumber
    }

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { number = genSnapshotNumber
    , utxo = healthyCloseUTxO
    , confirmed = []
    }

healthyCloseUTxO :: UTxO
healthyCloseUTxO =
  (genOneUTxOFor somePartyCardanoVerificationKey `suchThat` (/= healthyUTxO))
    `generateWith` 42

genSnapshotNumber :: SnapshotNumber
genSnapshotNumber = genSnapShot `generateWith` 42

healthyCloseDatum :: Head.State
healthyCloseDatum =
  Head.Open
    { parties = healthyOnChainParties
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
    , contestationPeriod = healthyContestationPeriod
    }

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger genContestationPeriodSeconds

-- NB: We don't want to wait too long for contestation period to pass
-- so constraining this to < 100
genContestationPeriodSeconds :: Integer
genContestationPeriodSeconds = arbitrary `suchThat` (< 100) `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey = flip generateWith 42 $ do
  genForParty genVerificationKey <$> elements healthyParties

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: SnapshotNumber -> MultiSignature (Snapshot Tx)
healthySignature number = aggregate [sign sk snapshot | sk <- healthySigningKeys]
 where
  snapshot = healthySnapshot{number}

data CloseMutation
  = MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotToIllFormedValue
  | MutateParties
  | MutateRequiredSigner
  | MutateCloseUTxOHash
  | -- | Mutate the tx upper bound, which makes it inconsistent with the deadline
    -- set in the output datum.
    MutateValidityIntervalToBeInconsistent
  | -- | Mutate the output datum's contestation deadline such that it is
    -- inconsistent with the upper bound set on the transaction.
    MutateContestationDeadlineToBeInconsistent
  | -- | Mutate the upper tx validity bound and output datum (consistently!) to
    -- be much further in the future than the agreed contestation period.
    MutateUpperValidityToBeFarInTheFuture
  deriving (Generic, Show, Enum, Bounded)

genCloseMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseMutation (tx, _utxo) =
  -- FIXME: using 'closeRedeemer' here is actually too high-level and reduces
  -- the power of the mutators, we should test at the level of the validator.
  -- That is, using the on-chain types. 'closeRedeemer' is also not used
  -- anywhere after changing this and can be moved into the closeTx
  oneof
    [ SomeMutation MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        closeRedeemer (number healthySnapshot) <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
    , SomeMutation MutateSnapshotNumberButNotSignature . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (\n -> n /= genSnapshotNumber && n > 0)
        pure (closeRedeemer mutatedSnapshotNumber $ healthySignature genSnapshotNumber)
    , SomeMutation MutateSnapshotToIllFormedValue . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
        let mutatedSignature =
              aggregate [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthySigningKeys]
        pure
          Head.Close
            { snapshotNumber = mutatedSnapshotNumber
            , signature = toPlutusSignatures mutatedSignature
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $
          Head.Open
            { parties = mutatedParties
            , utxoHash = ""
            , contestationPeriod = healthyContestationPeriod
            }
    , SomeMutation MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation MutateCloseUTxOHash . ChangeOutput 0 <$> mutateCloseUTxOHash
    , SomeMutation MutateContestationDeadlineToBeInconsistent . ChangeOutput 0 <$> mutateClosedContestationDeadline
    , SomeMutation MutateValidityIntervalToBeInconsistent . ChangeValidityInterval <$> do
        lb <- arbitrary
        ub <- arbitrary `suchThat` (/= TxValidityUpperBound healthyCloseUpperBoundSlotNo)
        pure (lb, ub)
    , SomeMutation MutateUpperValidityToBeFarInTheFuture <$> do
        muchGreaterThanContestationPeriod <-
          SlotNo . fromIntegral
            <$> arbitrary `suchThat` (> genContestationPeriodSeconds)
        let mutatedUpperBound = healthyCloseUpperBoundSlotNo + muchGreaterThanContestationPeriod
            deadlineUTCTime = addUTCTime (fromInteger genContestationPeriodSeconds) (slotNoToUTCTime mutatedUpperBound)
        let doMutation = \case
              Head.Closed{snapshotNumber, utxoHash, parties} ->
                Head.Closed
                  { snapshotNumber
                  , utxoHash
                  , parties
                  , contestationDeadline = posixFromUTCTime deadlineUTCTime
                  }
              st -> error $ "unexpected state " <> show st
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityNoLowerBound, TxValidityUpperBound mutatedUpperBound)
            , ChangeOutput 0 $ changeHeadOutputDatum doMutation headTxOut
            ]
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  closeRedeemer snapshotNumber sig =
    Head.Close
      { snapshotNumber = toInteger snapshotNumber
      , signature = toPlutusSignatures sig
      , utxoHash = ""
      }

  mutateCloseUTxOHash :: Gen (TxOut CtxTx)
  mutateCloseUTxOHash = do
    mutatedUTxOHash <- genHash
    pure $ changeHeadOutputDatum (mutateHash mutatedUTxOHash) headTxOut

  mutateHash mutatedUTxOHash = \case
    Head.Closed{snapshotNumber, parties, contestationDeadline} ->
      Head.Closed
        { snapshotNumber
        , utxoHash = toBuiltin mutatedUTxOHash
        , parties
        , contestationDeadline
        }
    st -> error $ "unexpected state " <> show st

  mutateClosedContestationDeadline :: Gen (TxOut CtxTx)
  mutateClosedContestationDeadline = do
    -- NOTE: we need to be sure the generated contestation period is large enough to have an impact on the on-chain
    -- deadline computation, which means having a resolution of seconds instead of the default picoseconds
    contestationPeriodSeconds <- arbitrary @Integer `suchThat` (/= genContestationPeriodSeconds)
    pure $ changeHeadOutputDatum (mutateContestationDeadline contestationPeriodSeconds) headTxOut

  mutateContestationDeadline contestationPeriod = \case
    Head.Closed{snapshotNumber, utxoHash, parties} ->
      Head.Closed
        { snapshotNumber
        , utxoHash
        , parties
        , contestationDeadline =
            let closingTime = slotNoToUTCTime healthyCloseUpperBoundSlotNo
             in posixFromUTCTime $ addUTCTime (fromInteger contestationPeriod) closingTime
        }
    st -> error $ "unexpected state " <> show st
