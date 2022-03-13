let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;

          # Disable Haddock generation and profiling by default. The former
          # can be done via cabal, while the latter should be enabled on
          # demand.
          defaultMod = drv:
            super.haskell.lib.dontHaddock
            (super.haskell.lib.disableLibraryProfiling drv);

          co-log-polysemy =
            defaultMod (super.haskell.lib.doJailbreak hsuper.co-log-polysemy);

          polysemy-utils-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          polysemy-utils =
            hself.callCabal2nix "polysemy-utils" polysemy-utils-src { };
        in {
          inherit polysemy-utils;
          inherit co-log-polysemy;
        };
    };
  };
in [ customHaskellPackages ]
