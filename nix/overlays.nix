let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;

          polysemy-utils-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          polysemy-utils =
            hself.callCabal2nix "polysemy-utils" polysemy-utils-src { };
        in { inherit polysemy-utils; };
    };
  };
in [ customHaskellPackages ]
