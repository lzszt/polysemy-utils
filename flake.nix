{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import ./nix/pkgs.nix { inherit nixpkgs system; };
        packageName = "polysemy-utils";
      in {
        packages.${packageName} = pkgs.haskellPackages.polysemy-utils;
        defaultPackage = self.packages.${system}.${packageName};
        devShells = {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.polysemy-utils ];
            nativeBuildInputs = with pkgs; [
              haskellPackages.cabal-install
              haskellPackages.ghc
              haskellPackages.hlint
              haskellPackages.ghcid
              haskellPackages.haskell-language-server
              haskellPackages.fourmolu
            ];
          };
        };
      });
}
