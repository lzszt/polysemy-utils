{ path ? null }:

let
  overlays = import ./overlays.nix;
  config = { allowBroken = true; };
  src = if isNull path then import ./pinned-nixpkgs.nix else path;
  pkgs = import src {
    inherit config;
    inherit overlays;
  };
in pkgs
