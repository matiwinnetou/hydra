{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = {
        name = "cardano-ledger-babbage-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Tests for Cardano ledger babbage era";
      description = "This package builds upon the Alonzo ledger";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "cardano-ledger-babbage-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-babbage-test-0.1.0.0.tar.gz";
      sha256 = "2a03cf1e85c4c0eb8e2054a799df50d9429c0dd20296c8edd0d908eac4066157";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname:                cardano-ledger-babbage-test\nversion:             0.1.0.0\nsynopsis:            Tests for Cardano ledger babbage era\ndescription:\n  This package builds upon the Alonzo ledger\nbug-reports:         https://github.com/input-output-hk/cardano-ledger/issues\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncopyright:           2020 Input Output (Hong Kong) Ltd.\ncategory:            Network\nbuild-type:          Simple\n\nextra-source-files:\n  cddl-files/babbage.cddl\n  cddl-files/real/crypto.cddl\n  cddl-files/mock/extras.cddl\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   eras/babbage/test-suite\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wpartial-fields\n                      -Wredundant-constraints\n                      -- -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:\n    Test.Cardano.Ledger.Babbage.Examples.Consensus,\n    Test.Cardano.Ledger.Babbage.Serialisation.Generators\n  build-depends:\n    bytestring,\n    cardano-binary,\n    cardano-ledger-alonzo,\n    cardano-ledger-alonzo-test,\n    cardano-ledger-babbage,\n    cardano-ledger-core,\n    cardano-ledger-pretty,\n    cardano-ledger-shelley-ma-test,\n    cardano-ledger-shelley-ma,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    containers,\n    data-default-class,\n    hashable,\n    plutus-tx,\n    plutus-ledger-api,\n    QuickCheck,\n    cardano-ledger-shelley-test,\n    cardano-ledger-shelley,\n    small-steps,\n    strict-containers,\n    text,\n  hs-source-dirs:\n    src\n\ntest-suite cardano-ledger-babbage-test\n  import:             base, project-config\n\n  type:                exitcode-stdio-1.0\n  main-is:             Tests.hs\n  hs-source-dirs:\n    test\n  other-modules:\n    Test.Cardano.Ledger.Babbage.Serialisation.Tripping,\n    Test.Cardano.Ledger.Babbage.Serialisation.CDDL,\n    Test.Cardano.Ledger.Babbage.TxInfo\n  build-depends:\n    QuickCheck,\n    base16-bytestring,\n    bytestring,\n    cardano-binary,\n    cardano-data,\n    cardano-ledger-alonzo,\n    cardano-ledger-alonzo-test,\n    cardano-ledger-babbage,\n    cardano-ledger-babbage-test,\n    cardano-ledger-core,\n    cardano-ledger-pretty,\n    cardano-ledger-shelley,\n    cardano-ledger-shelley-ma,\n    cardano-ledger-shelley-ma-test,\n    cardano-ledger-shelley-test,\n    cardano-protocol-tpraos,\n    cardano-slotting,\n    cborg,\n    vector-map,\n    containers,\n    data-default-class,\n    plutus-core,\n    plutus-ledger-api,\n    plutus-tx,\n    small-steps,\n    small-steps-test,\n    strict-containers,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    time,\n    pretty-show,\n    transformers\n";
    }