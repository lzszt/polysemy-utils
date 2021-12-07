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

          # A recent version of polysemy.
          polysemySrc = super.fetchFromGitHub {
            owner = "polysemy-research";
            repo = "polysemy";
            rev = "c37d485b614e98622f5e7473a478b781a6ad5c45";
            sha256 = "0y8i5c4k4q75qmq5brxdqpa39zzgywcv9ipbvz9hkd6av6f8673r";
          };

          # Use the newest version of polysemy to circumvent dependency
          # version problems.
          polysemy =
            defaultMod (hself.callCabal2nix "polysemy" polysemySrc { });
          polysemy-plugin = defaultMod
            (hself.callCabal2nix "polysemy" (polysemySrc + /polysemy-plugin)
              { });

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
          inherit polysemy;
          inherit polysemy-plugin;
          inherit co-log-polysemy;
        };
    };
  };
in [ customHaskellPackages ]
