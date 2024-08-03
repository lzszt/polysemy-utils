{ nixpkgs, system }:

let
  overlays = import ./overlays.nix;
  config = {
    allowBroken = false;
  };
in
import nixpkgs {
  inherit config;
  inherit overlays;
  inherit system;
}
