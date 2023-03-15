-- | Mutation-based script validator tests for the commit transaction where a
-- 'healthyCommitTx' gets mutated by an arbitrary 'CommitMutation'.
module Hydra.Chain.Direct.Contract.Commit where

import Hydra.Cardano.Api
import Hydra.Prelude

-- Arbitrary VerificationKey instance
import Hydra.Chain.Direct.TxSpec ()

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.List as List
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Gen (genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  replacePolicyIdWith,
 )
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (commitTx, mkHeadId, mkInitialOutput)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadTokens (headPolicyId)
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.InitialError (InitialError (..))
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import Hydra.Ledger.Cardano (
  genAddressInEra,
  genOutput,
  genValue,
  genVerificationKey,
 )
import Hydra.Party (Party)
import Test.QuickCheck (oneof, scale, suchThat)

--
-- CommitTx
--

healthyCommitTx :: (Tx, UTxO)
healthyCommitTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (healthyIntialTxIn, toUTxOContext healthyInitialTxOut)
      <> healthyCommittedUTxO
      <> registryUTxO scriptRegistry
  tx =
    commitTx
      Fixture.testNetworkId
      scriptRegistry
      (mkHeadId Fixture.testPolicyId)
      commitParty
      healthyCommittedUTxO
      (healthyIntialTxIn, toUTxOContext healthyInitialTxOut, initialPubKeyHash)

  scriptRegistry = genScriptRegistry `generateWith` 42

  initialPubKeyHash = verificationKeyHash commitVerificationKey

  commitParty :: Party
  commitParty = generateWith arbitrary 42

commitVerificationKey :: VerificationKey PaymentKey
commitVerificationKey = generateWith arbitrary 42

healthyIntialTxIn :: TxIn
healthyIntialTxIn = generateWith arbitrary 42

healthyInitialTxOut :: TxOut CtxTx
healthyInitialTxOut = mkInitialOutput Fixture.testNetworkId Fixture.testSeedInput commitVerificationKey

-- NOTE: An 8₳ output which is currently addressed to some arbitrary key.
healthyCommittedUTxO :: UTxO
healthyCommittedUTxO = flip generateWith 42 $ do
  txIn <- arbitrary
  txOut <- modifyTxOutValue (const $ lovelaceToValue 8_000_000) <$> (genOutput =<< arbitrary)
  pure $ UTxO.singleton (txIn, txOut)

data CommitMutation
  = -- | Invalidates the tx by changing the commit output value.
    --
    -- Ensures the committed value is consistent with the locked value by the
    -- commit validator.
    MutateCommitOutputValue
  | -- | Invalidates the tx by changing the value of the input committed utxo.
    --
    -- Ensures the output committed utxo value is consistent with the input committed utxo value.
    MutateCommittedValue
  | -- | Invalidates the tx by changing the address of the input out-ref.
    --
    -- Ensures the output tx out-ref is consistent with the input tx out-ref.
    MutateCommittedAddress
  | -- | Ensures commit is authenticated by a Head party by changing the signer
    -- used on the transaction to be the one in the PT.
    MutateRequiredSigner
  | -- | Change the head policy id to simulate commit using a PT and signer from
    -- a different head. The signer shows a correct signature but from a
    -- different head. This will cause the signer to not be present in the
    -- participation tokens.
    CommitFromDifferentHead
  | -- | Minting or burning of the tokens should not be possible in commit.
    MutateTokenMintingOrBurning
  deriving (Generic, Show, Enum, Bounded)

genCommitMutation :: (Tx, UTxO) -> Gen SomeMutation
genCommitMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode LockedValueDoesNotMatch) MutateCommitOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- scale (`div` 2) genValue `suchThat` (/= commitOutputValue)
        pure $ commitTxOut{txOutValue = mutatedValue}
    , SomeMutation (Just $ toErrorCode LockedValueDoesNotMatch) MutateCommittedValue <$> do
        mutatedValue <- scale (`div` 2) genValue `suchThat` (/= committedOutputValue)
        let mutatedOutput = modifyTxOutValue (const mutatedValue) committedTxOut
        pure $ ChangeInput committedTxIn mutatedOutput Nothing
    , SomeMutation (Just $ toErrorCode MismatchCommittedTxOutInDatum) MutateCommittedAddress <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId `suchThat` (/= committedAddress)
        let mutatedOutput = modifyTxOutAddress (const mutatedAddress) committedTxOut
        pure $ ChangeInput committedTxIn mutatedOutput Nothing
    , SomeMutation (Just $ toErrorCode MissingOrInvalidCommitAuthor) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , -- XXX: This is a bit confusing and not giving much value. Maybe we can remove this.
      -- This also seems to be covered by MutateRequiredSigner
      SomeMutation (Just $ toErrorCode CouldNotFindTheCorrectCurrencySymbolInTokens) CommitFromDifferentHead <$> do
        otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= healthyIntialTxIn))
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId commitTxOut)
            , ChangeInput
                healthyIntialTxIn
                (toUTxOContext $ replacePolicyIdWith Fixture.testPolicyId otherHeadId healthyInitialTxOut)
                (Just $ toScriptData $ Initial.ViaCommit [toPlutusTxOutRef committedTxIn])
            ]
    , SomeMutation (Just $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    ]
 where
  TxOut{txOutValue = commitOutputValue} = commitTxOut

  commitTxOut = fromJust $ txOuts' tx !!? 0

  (committedTxIn, committedTxOut) = List.head $ UTxO.pairs healthyCommittedUTxO

  committedAddress = txOutAddress committedTxOut

  committedOutputValue = txOutValue committedTxOut
