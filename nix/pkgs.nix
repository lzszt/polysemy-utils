{ nixpkgs, system }:

let
  overlays = import ./overlays.nix;
  config = { allowBroken = false; };

  pkgs = import nixpkgs {
    inherit config;
    inherit overlays;
    inherit system;
  };
in pkgs
