module Hydra.API.Crypto where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Crypto.DSIGN (
  ContextDSIGN,
  DSIGNAlgorithm (signDSIGN),
  Ed25519DSIGN,
  SigDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
  algorithmNameDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  hashVerKeyDSIGN,
  rawDeserialiseSigDSIGN,
  rawDeserialiseSignKeyDSIGN,
  rawDeserialiseVerKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseSignKeyDSIGN,
  rawSerialiseVerKeyDSIGN,
  seedSizeDSIGN,
  verifyDSIGN,
 )
import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash (Blake2b_256, SHA256, castHash, digest, hashFromBytes, hashToBytes)
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Crypto.Seed (getSeedBytes, mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Proxy (Proxy)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Hydra.Cardano.Api (AsType (..), HasTextEnvelope, HasTypeProxy (..), Hash, SerialiseAsCBOR, SerialiseAsRawBytes, serialiseToRawBytesHexText)
import Hydra.Cardano.Api.Prelude (HasTextEnvelope (..), Key (..), Proxy (..), SerialiseAsRawBytes (..))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances.ByteString ()

-- * Keys

-- | Hydra keys (keyrole) which can be used to 'sign' and 'verify' messages, as
-- well as 'aggregate' multi-signatures.
data HydraKey

instance HasTypeProxy HydraKey where
  data AsType HydraKey = AsHydraKey
  proxyToAsType _ = AsHydraKey

-- * Hydra keys

-- | Hashes of Hydra keys
newtype instance Hash HydraKey
  = HydraKeyHash (Crypto.Hash Blake2b_256 (VerificationKey HydraKey))
  deriving stock (Ord, Eq, Show)

instance SerialiseAsRawBytes (Hash HydraKey) where
  serialiseToRawBytes (HydraKeyHash vkh) = hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsHydraKey) bs =
    HydraKeyHash <$> hashFromBytes bs

instance Key HydraKey where
  -- Hydra verification key, which can be used to 'verify' signed messages.
  newtype VerificationKey HydraKey
    = HydraVerificationKey (VerKeyDSIGN Ed25519DSIGN)
    deriving (Eq, Show)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (SerialiseAsCBOR)

  -- Hydra signing key which can be used to 'sign' messages and 'aggregate'
  -- multi-signatures or 'deriveVerificationKey'.
  --
  -- REVIEW: Maybe rewrite Show instance to /not/ expose secret, eg. 8 bytes
  -- from the hash of the key? Although both, cardano-api and
  -- cardano-crypto-class are both deriving this and thus showing secret key
  -- material as well.
  newtype SigningKey HydraKey
    = HydraSigningKey (SignKeyDSIGN Ed25519DSIGN)
    deriving (Eq, Show)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (SerialiseAsCBOR)

  -- Get the 'VerificationKey' for a given 'SigningKey'.
  getVerificationKey (HydraSigningKey sk) =
    HydraVerificationKey $ deriveVerKeyDSIGN sk

  -- Create a new 'SigningKey' from a 'Seed'. See 'generateSigningKey'
  deterministicSigningKey AsHydraKey =
    generateSigningKey . getSeedBytes

  -- Get the number of bytes required to seed a signing key with
  -- 'deterministicSigningKey'.
  deterministicSigningKeySeedSize AsHydraKey =
    seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)

  -- Get the verification key hash of a 'VerificationKey'. See 'Blake2b_256' for
  -- info on the used hashing algorithm.
  verificationKeyHash (HydraVerificationKey vk) =
    HydraKeyHash . castHash $ hashVerKeyDSIGN vk

instance Arbitrary (SigningKey HydraKey) where
  arbitrary = generateSigningKey <$> arbitrary

instance SerialiseAsRawBytes (SigningKey HydraKey) where
  serialiseToRawBytes (HydraSigningKey sk) =
    rawSerialiseSignKeyDSIGN sk

  deserialiseFromRawBytes (AsSigningKey AsHydraKey) bs =
    HydraSigningKey <$> rawDeserialiseSignKeyDSIGN bs

instance HasTextEnvelope (SigningKey HydraKey) where
  textEnvelopeType _ =
    "HydraSigningKey_"
      <> fromString (algorithmNameDSIGN (Proxy :: Proxy Ed25519DSIGN))

instance Arbitrary (VerificationKey HydraKey) where
  arbitrary = getVerificationKey . generateSigningKey <$> arbitrary

instance SerialiseAsRawBytes (VerificationKey HydraKey) where
  serialiseToRawBytes (HydraVerificationKey vk) =
    rawSerialiseVerKeyDSIGN vk

  deserialiseFromRawBytes (AsVerificationKey AsHydraKey) bs =
    HydraVerificationKey <$> rawDeserialiseVerKeyDSIGN bs

instance ToJSON (VerificationKey HydraKey) where
  toJSON = toJSON . serialiseToRawBytesHexText

-- TODO: It would be nice(r) to have a bech32 representation for verification
-- keys BUT cardano-api decided to not expose the class internals which makes it
-- impossible to define new instances for that class :upside-down-smiling-face:
--
-- instance SerialiseAsBech32 VerificationKey where
--  bech32PrefixFor = const "hydra_vk"
--  bech32PrefixesPermitted _ = ["hydra_vk"]

instance FromJSON (VerificationKey HydraKey) where
  parseJSON = Aeson.withText "VerificationKey" $ decodeBase16 >=> deserialiseKey
   where
    deserialiseKey =
      maybe
        (fail "unable to deserialize VerificationKey, wrong length")
        (pure . HydraVerificationKey)
        . rawDeserialiseVerKeyDSIGN

instance HasTextEnvelope (VerificationKey HydraKey) where
  textEnvelopeType _ =
    "HydraVerificationKey_"
      <> fromString (algorithmNameDSIGN (Proxy :: Proxy Ed25519DSIGN))

-- | Create a new 'SigningKey' from a 'ByteString' seed. The created keys are
-- not random and insecure, so don't use this in production code!
generateSigningKey :: ByteString -> SigningKey HydraKey
generateSigningKey seed =
  HydraSigningKey . genKeyDSIGN $ mkSeedFromBytes hashOfSeed
 where
  hashOfSeed = digest (Proxy :: Proxy SHA256) seed

-- * Signatures

-- | Signature of 'a', not containing the actual payload.
newtype Signature a = HydraSignature (SigDSIGN Ed25519DSIGN)
  deriving (Eq)
  deriving newtype (ToCBOR, FromCBOR)

instance Show (Signature a) where
  show (HydraSignature sig) =
    "HydraSignature " <> show hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseSigDSIGN sig

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (Signature a) where
  arbitrary = sign <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (Signature a) where
  toJSON (HydraSignature sig) = Aeson.String $ decodeUtf8 hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseSigDSIGN sig

instance FromJSON a => FromJSON (Signature a) where
  parseJSON = Aeson.withText "Signed" $ \t -> do
    bs <- decodeBase16 t
    maybe
      (fail "deserialise signature from bytes failed")
      (pure . HydraSignature)
      $ rawDeserialiseSigDSIGN bs

-- | Sign some value 'a' with the provided 'SigningKey'.
sign :: SignableRepresentation a => SigningKey HydraKey -> a -> Signature a
sign (HydraSigningKey sk) a =
  HydraSignature $ signDSIGN ctx a sk
 where
  ctx = () :: ContextDSIGN Ed25519DSIGN

-- | Verify a given 'Signature a' and value 'a' using provided 'VerificationKey'.
verify ::
  SignableRepresentation a =>
  VerificationKey HydraKey ->
  Signature a ->
  a ->
  Bool
verify (HydraVerificationKey vk) (HydraSignature sig) a =
  case verifyDSIGN ctx vk a sig of
    Right () -> True
    -- NOTE: Current implementation does not yield multiple Left cases, so no need
    -- to distinguish in our interface
    Left _ -> False
 where
  ctx = () :: ContextDSIGN Ed25519DSIGN

-- * Multi-signatures

-- | Naiively aggregated multi-signatures.
newtype MultiSignature a = HydraMultiSignature {multiSignature :: [Signature a]}
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (ToJSON, FromJSON)

-- | Combine multiple signatures of 'a' into a 'MultiSignature a'.
aggregate :: [Signature a] -> MultiSignature a
aggregate = HydraMultiSignature

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (MultiSignature a) where
  arbitrary = HydraMultiSignature <$> arbitrary

-- * Utility

decodeBase16 :: Text -> Parser ByteString
decodeBase16 = either fail pure . Base16.decode . encodeUtf8