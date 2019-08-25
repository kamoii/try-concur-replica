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
      identifier = { name = "try-concur-replica"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 Author name here";
      maintainer = "example@example.com";
      author = "Author name here";
      homepage = "https://github.com/githubuser/try-concur-replica#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/try-concur-replica#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (buildDepError "array"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."concur-core" or (buildDepError "concur-core"))
          (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
          (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
          (hsPkgs."relude" or (buildDepError "relude"))
          (hsPkgs."replica" or (buildDepError "replica"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."websockets" or (buildDepError "websockets"))
          ];
        };
      exes = {
        "spla-league-random-match" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."discord-haskell" or (buildDepError "discord-haskell"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
            (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
            (hsPkgs."pretty-simple" or (buildDepError "pretty-simple"))
            (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."relude" or (buildDepError "relude"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            (hsPkgs."try-concur-replica" or (buildDepError "try-concur-replica"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "try-concur-replica-button-pusher" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
            (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."relude" or (buildDepError "relude"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            (hsPkgs."try-concur-replica" or (buildDepError "try-concur-replica"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "try-concur-replica-event-lost" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
            (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
            (hsPkgs."relude" or (buildDepError "relude"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            (hsPkgs."try-concur-replica" or (buildDepError "try-concur-replica"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "try-concur-replica-phantom-event" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
            (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
            (hsPkgs."relude" or (buildDepError "relude"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            (hsPkgs."try-concur-replica" or (buildDepError "try-concur-replica"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        "try-concur-replica-routing" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
            (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
            (hsPkgs."relude" or (buildDepError "relude"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            (hsPkgs."try-concur-replica" or (buildDepError "try-concur-replica"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        };
      tests = {
        "try-concur-replica-test" = {
          depends = [
            (hsPkgs."array" or (buildDepError "array"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."concur-core" or (buildDepError "concur-core"))
            (hsPkgs."concur-replica" or (buildDepError "concur-replica"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."pcre-heavy" or (buildDepError "pcre-heavy"))
            (hsPkgs."product-profunctors" or (buildDepError "product-profunctors"))
            (hsPkgs."relude" or (buildDepError "relude"))
            (hsPkgs."replica" or (buildDepError "replica"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            (hsPkgs."try-concur-replica" or (buildDepError "try-concur-replica"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }) // {
    cabal-generator = "hpack";
    }