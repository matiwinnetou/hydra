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
    flags = {
      bytestring-builder = false;
      cffi = false;
      ordered-keymap = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "aeson"; version = "2.0.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "Adam Bergmark <adam@bergmark.nl>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/aeson";
      url = "";
      synopsis = "Fast JSON parsing and encoding";
      description = "A JSON parsing and encoding library optimized for ease of use\nand high performance.\n\nTo get started, see the documentation for the @Data.Aeson@ module\nbelow.\n\n(A note on naming: in Greek mythology, Aeson was the father of Jason.)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"));
        buildable = true;
        };
      tests = {
        "aeson-tests" = {
          depends = (([
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (if flags.bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
            (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/aeson-2.0.2.0.tar.gz";
      sha256 = "d5ca55dd7fab55d3a0e166f04d14d1361696077b664b58b42e2523a160208037";
      });
    }) // {
    package-description-override = "name:               aeson\nversion:            2.0.2.0\nx-revision: 1\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Text, Web, JSON\ncopyright:\n  (c) 2011-2016 Bryan O'Sullivan\n  (c) 2011 MailRank, Inc.\n\nauthor:             Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:         Adam Bergmark <adam@bergmark.nl>\nstability:          experimental\ntested-with:\n  GHC ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.1\n\nsynopsis:           Fast JSON parsing and encoding\ncabal-version:      >=1.10\nhomepage:           https://github.com/haskell/aeson\nbug-reports:        https://github.com/haskell/aeson/issues\nbuild-type:         Simple\ndescription:\n  A JSON parsing and encoding library optimized for ease of use\n  and high performance.\n  .\n  To get started, see the documentation for the @Data.Aeson@ module\n  below.\n  .\n  (A note on naming: in Greek mythology, Aeson was the father of Jason.)\n\nextra-source-files:\n  *.yaml\n  benchmarks/json-data/*.json\n  cbits/*.c\n  changelog.md\n  README.markdown\n  src-ffi/Data/Aeson/Parser/*.hs\n  src-pure/Data/Aeson/Parser/*.hs\n  tests/golden/*.expected\n  tests/JSONTestSuite/test_parsing/*.json\n  tests/JSONTestSuite/test_transform/*.json\n\nflag bytestring-builder\n  description:\n    Depend on the bytestring-builder package for backwards compatibility.\n\n  default:     False\n  manual:      False\n\nflag cffi\n  description:\n    Controls whether to include c-ffi bits or pure haskell. Default to False for security.\n\n  default:     False\n  manual:      True\n\nflag ordered-keymap\n  description: Use ordered @Data.Map.Strict@ for KeyMap implementation.\n  default:     True\n  manual:      True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src attoparsec-iso8601/src\n  exposed-modules:\n    Data.Aeson\n    Data.Aeson.Encoding\n    Data.Aeson.Encoding.Internal\n    Data.Aeson.Internal\n    Data.Aeson.Internal.Time\n    Data.Aeson.Key\n    Data.Aeson.KeyMap\n    Data.Aeson.Parser\n    Data.Aeson.Parser.Internal\n    Data.Aeson.QQ.Simple\n    Data.Aeson.Text\n    Data.Aeson.TH\n    Data.Aeson.Types\n\n  other-modules:\n    Data.Aeson.Encoding.Builder\n    Data.Aeson.Internal.Functions\n    Data.Aeson.Parser.Time\n    Data.Aeson.Parser.Unescape\n    Data.Aeson.Types.Class\n    Data.Aeson.Types.FromJSON\n    Data.Aeson.Types.Generic\n    Data.Aeson.Types.Internal\n    Data.Aeson.Types.ToJSON\n    Data.Attoparsec.Time\n    Data.Attoparsec.Time.Internal\n\n  -- GHC bundled libs\n  build-depends:\n      base              >=4.9.0.0  && <5\n    , bytestring        >=0.10.8.1 && <0.11.2\n    , containers        >=0.5.7.1  && <0.7\n    , deepseq           >=1.4.2.0  && <1.5\n    , ghc-prim          >=0.5.0.0  && <0.9\n    , template-haskell  >=2.11.0.0 && <2.19\n    , text              >=1.2.3.0  && <1.3\n    , time              >=1.6.0.1  && <1.13\n\n  -- Compat\n  build-depends:\n      base-compat-batteries  >=0.10.0 && <0.13\n    , time-compat            >=1.9.6  && <1.10\n\n  if !impl(ghc >=8.6)\n    build-depends: contravariant >=1.4.1 && <1.6\n\n  -- Other dependencies\n  build-depends:\n      attoparsec            >=0.13.2.2 && <0.15\n    , data-fix              >=0.3      && <0.4\n    , dlist                 >=0.8.0.4  && <1.1\n    , hashable              >=1.3.5.0  && <1.5\n    , indexed-traversable   >=0.1.1    && <0.2\n    , OneTuple              >=0.3.1    && <0.4\n    , primitive             >=0.7.0.1  && <0.8\n    , scientific            >=0.3.7.0  && <0.4\n    , semialign             >=1.2      && <1.3\n    , strict                >=0.4      && <0.5\n    , tagged                >=0.8.6    && <0.9\n    , text-short            >=0.1.4    && <0.2\n    , th-abstraction        >=0.3.0.0  && <0.5\n    , these                 >=1.1      && <1.2\n    , unordered-containers  >=0.2.10.0 && <0.3\n    , uuid-types            >=1.0.3    && <1.1\n    , vector                >=0.12.0.1 && <0.13\n    , witherable            >=0.4.1    && <0.5\n\n  ghc-options:      -Wall\n\n  if (impl(ghcjs) || !flag(cffi))\n    hs-source-dirs: src-pure\n    other-modules:  Data.Aeson.Parser.UnescapePure\n\n  else\n    c-sources:      cbits/unescape_string.c\n    cpp-options:    -DCFFI\n    hs-source-dirs: src-ffi\n    other-modules:  Data.Aeson.Parser.UnescapeFFI\n\n  if flag(ordered-keymap)\n    cpp-options: -DUSE_ORDEREDMAP=1\n\ntest-suite aeson-tests\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests src-ffi src-pure\n  main-is:          Tests.hs\n  c-sources:        cbits/unescape_string.c\n  ghc-options:      -Wall -threaded -rtsopts\n  other-modules:\n    Data.Aeson.Parser.UnescapeFFI\n    Data.Aeson.Parser.UnescapePure\n    DataFamilies.Encoders\n    DataFamilies.Instances\n    DataFamilies.Properties\n    DataFamilies.Types\n    Encoders\n    ErrorMessages\n    Functions\n    Instances\n    Options\n    Properties\n    PropertyGeneric\n    PropertyKeys\n    PropertyRoundTrip\n    PropertyRTFunctors\n    PropertyTH\n    PropUtils\n    SerializationFormatSpec\n    Types\n    UnitTests\n    UnitTests.NullaryConstructors\n\n  build-depends:\n      aeson\n    , attoparsec\n    , base\n    , base-compat\n    , base-orphans          >=0.5.3    && <0.9\n    , base16-bytestring\n    , containers\n    , data-fix\n    , Diff                  >=0.4      && <0.5\n    , directory\n    , dlist\n    , filepath\n    , generic-deriving      >=1.10     && <1.15\n    , ghc-prim              >=0.2\n    , hashable\n    , integer-logarithms    >=1        && <1.1\n    , OneTuple\n    , QuickCheck            >=2.14.2   && <2.15\n    , quickcheck-instances  >=0.3.26.1 && <0.4\n    , scientific\n    , strict\n    , tagged\n    , tasty\n    , tasty-golden\n    , tasty-hunit\n    , tasty-quickcheck\n    , template-haskell\n    , text\n    , text-short\n    , these\n    , time\n    , time-compat\n    , unordered-containers\n    , uuid-types\n    , vector\n\n  if flag(bytestring-builder)\n    build-depends:\n        bytestring          >=0.9    && <0.10.4\n      , bytestring-builder  >=0.10.4 && <1\n\n  else\n    build-depends: bytestring >=0.10.4\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups           >=0.18.2  && <0.20\n      , transformers         >=0.2.2.0\n      , transformers-compat  >=0.3\n\n  if !impl(ghc >=7.10)\n    build-depends:\n        nats  >=1     && <1.2\n      , void  >=0.7.2 && <0.8\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/aeson.git\n";
    }