{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = import ./default.nix { pkgs = pkgs; };
in
  pkgs.dockerTools.buildImage {
    name = "spla-league-match";
    contents = [
      hsPkgs.try-concur-replica.components.exes.spla-league-random-match
      pkgs.iana-etc
      pkgs.cacert
    ];
    config.Cmd = [ "/bin/spla-league-random-match" ];
  }
