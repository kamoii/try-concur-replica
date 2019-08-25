let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "replica"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 (c) Philip Kamenarsky";
      maintainer = "p.kamenarsky@gmail.com";
      author = "Philip Kamenarsky";
      homepage = "https://github.com/https://github.com/pkamenarsky/replica#readme";
      url = "";
      synopsis = "Remote virtual DOM library";
      description = "Please see the README on GitHub at <https://github.com/pkamenarsky/replica#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Diff" or (buildDepError "Diff"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."chronos" or (buildDepError "chronos"))
          (hsPkgs."co-log-core" or (buildDepError "co-log-core"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."file-embed" or (buildDepError "file-embed"))
          (hsPkgs."hex-text" or (buildDepError "hex-text"))
          (hsPkgs."http-media" or (buildDepError "http-media"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."psqueues" or (buildDepError "psqueues"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."torsor" or (buildDepError "torsor"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
          (hsPkgs."websockets" or (buildDepError "websockets"))
          ];
        };
      tests = {
        "replica-test" = {
          depends = [
            (hsPkgs."Diff" or (buildDepError "Diff"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."chronos" or (buildDepError "chronos"))
            (hsPkgs."co-log-core" or (buildDepError "co-log-core"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."file-embed" or (buildDepError "file-embed"))
            (hsPkgs."hex-text" or (buildDepError "hex-text"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."psqueues" or (buildDepError "psqueues"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."torsor" or (buildDepError "torsor"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault /home/sino/mygithub/replica; }) // {
    cabal-generator = "hpack";
    }