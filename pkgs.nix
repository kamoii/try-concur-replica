{
  extras = hackage:
    {
      packages = ({
        "emoji" = (((hackage.emoji)."0.1.0.2").revisions)."d995572a5c7dcd28f98eb15c6e387a7b3bda1ac2477ab0d9dba8580d5d7b161f";
        "hex-text" = (((hackage.hex-text)."0.1.0.0").revisions)."52dc2cf5ce45f7d86ea5e56e2ac57820f788c10bbb3fe83f6d57e678a426305b";
        "chronos" = (((hackage.chronos)."1.0.6").revisions)."ff7399115163d345d24bdb98ed3e83042bb5eca901c7a6230c3d08727330ce94";
        "torsor" = (((hackage.torsor)."0.1").revisions)."9c6f152650322357bee013a224d9d7ec8e8ae9fe32d8cee45f1431c49b0129e2";
        } // {
        try-concur-replica = ./try-concur-replica.nix;
        concur-replica = ./concur-replica.nix;
        replica = ./replica.nix;
        concur-core = ./concur-core.nix;
        discord-haskell = ./discord-haskell.nix;
        websockets = ./websockets.nix;
        }) // {};
      };
  resolver = "lts-13.29";
  }