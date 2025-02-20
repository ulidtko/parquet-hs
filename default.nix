{ compiler ? "ghc9101" }:

let
  sources = import ./nix/sources.nix;
  # `pinch` is fixed on haskell-updates but it'll be a while before
  # it's backported and we prefer using the release branch
  pkgs = import sources.nixpkgs { config.allowBroken = true; };

  inherit (pkgs.haskell.lib) dontCheck doJailbreak;

  baseHaskellPkgs = pkgs.haskell.packages.${compiler};

  myHaskellPackages = baseHaskellPkgs.override {
    overrides = self: super: {
      parquet-hs = self.callCabal2nix "parquet-hs" (./.) { };
      # I opened up an MR fixing `pinch` on nixpkgs by bumping the version of
      # network `pinch` uses but it's causing issues here so instead we're
      # just disabling the test suite here as that's where the error is.
      pinch = dontCheck super.pinch;
      serialise = doJailbreak super.serialise;
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: with p; [ parquet-hs ];

    buildInputs = with baseHaskellPkgs; [
      cabal-install
      haskell-language-server
      hpack
      ormolu
      pkgs.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
  };

in {
  inherit shell;
  inherit myHaskellPackages;
  parquet-hs = myHaskellPackages.parquet-hs;
}
