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
      identifier = { name = "concur-replica"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 (C) All Rights Reserved.";
      maintainer = "p.kamenarsky@gmail.com";
      author = "Philip Kamenarsky";
      homepage = "https://github.com/pkamenarsky/concur-replica#readme";
      url = "";
      synopsis = "Replica backend for Concur.";
      description = "Replica backend for Concur.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."byteorder" or (buildDepError "byteorder"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."chronos" or (buildDepError "chronos"))
          (hsPkgs."co-log" or (buildDepError "co-log"))
          (hsPkgs."concur-core" or (buildDepError "concur-core"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."free" or (buildDepError "free"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."replica" or (buildDepError "replica"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."torsor" or (buildDepError "torsor"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."websockets" or (buildDepError "websockets"))
          ];
        };
      exes = {
        "concur-replica-calc" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."byteorder" or (buildDepError "byteorder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."chronos" or (buildDepError "chronos"))
            (hsPkgs."co-log" or (buildDepError "co-log"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."free" or (buildDepError "free"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."torsor" or (buildDepError "torsor"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "concur-replica-hilo" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."byteorder" or (buildDepError "byteorder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."chronos" or (buildDepError "chronos"))
            (hsPkgs."co-log" or (buildDepError "co-log"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."free" or (buildDepError "free"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."torsor" or (buildDepError "torsor"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "concur-replica-menu" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."byteorder" or (buildDepError "byteorder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."chronos" or (buildDepError "chronos"))
            (hsPkgs."co-log" or (buildDepError "co-log"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."free" or (buildDepError "free"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."torsor" or (buildDepError "torsor"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "concur-replica-misc" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."byteorder" or (buildDepError "byteorder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."chronos" or (buildDepError "chronos"))
            (hsPkgs."co-log" or (buildDepError "co-log"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."free" or (buildDepError "free"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."torsor" or (buildDepError "torsor"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "concur-replica-multi-entry" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."byteorder" or (buildDepError "byteorder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."chronos" or (buildDepError "chronos"))
            (hsPkgs."co-log" or (buildDepError "co-log"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."free" or (buildDepError "free"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."torsor" or (buildDepError "torsor"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault /home/sino/mygithub/concur-replica;
    }) // { cabal-generator = "hpack"; }