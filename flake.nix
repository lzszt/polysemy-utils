{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import ./nix/pkgs.nix {
        inherit nixpkgs;
        system = "x86_64-linux";
      };
      packageName = "polysemy-utils";
    in {
      packages.x86_64-linux.${packageName} =
        pkgs.haskellPackages.polysemy-utils;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.${packageName};
      devShells.x86_64-linux = {
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
    };
}
