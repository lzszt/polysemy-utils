{ path ? null }:

let pkgs = import ./nix/pkgs.nix { inherit path; };
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.polysemy-utils ];
  buildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.haskell-language-server
  ];
}
