self: super:

let

  # This is a specific revision of Nixpkgs that we use to avoid
  # rebuilding all the versions of solc when we bump our submodule, or
  # to allow a package to succeed when something breaks in nixpkgs.
  past = import (super.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "0bb2d3112b259940df18ec6c0203bb01234f4e48";
    sha256 = "110jcn1k0kc9jmcbg97av10m36i4mqyxa057hwl6lpzjhrq40f3k";
  }) { config = {}; };

  callPackage = self.pkgs.callPackage;
  pastPackage = past.pkgs.callPackage;

  stdenv = self.pkgs.stdenv;

  haskellPackages = super.pkgs.haskellPackages.override {
    overrides = (import ./haskell.nix { pkgs = super.pkgs; });
  };

  profilingHaskellPackages = haskellPackages.override {
    overrides = self: super-hs:
      (import ./haskell.nix { pkgs = super.pkgs; } self super-hs) // {
        mkDerivation = args: super-hs.mkDerivation
          (args // { enableLibraryProfiling = true; });
      };
  };

in rec {
  solc = callPackage ((import ./solc-versions.nix).solc_0_4_17) {};

  solc-versions =
    super.lib.mapAttrs
      (_: value: pastPackage value {})
      (import ./solc-versions.nix);

  python3 = python36;
  python36 = super.python36.override {
    packageOverrides = (import ./python.nix { pkgs = super.pkgs; });
  };

  hevm =
    self.pkgs.haskell.lib.justStaticExecutables
      (haskellPackages.callPackage ./pkgs/hevm.nix {});

  hevm-profiling =
    profilingHaskellPackages.callPackage ./pkgs/hevm.nix {};

  seth   = callPackage ./pkgs/seth.nix {};
  dapp   = callPackage ./pkgs/dapp.nix {};
  setzer = callPackage ./pkgs/setzer.nix {};
  keeper = callPackage ./pkgs/keeper.nix {};

  go-ethereum = super.go-ethereum.overrideDerivation (_: rec {
    name = "go-ethereum-${version}";
    version = "1.7.1";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "1rhqnqp2d951d4084z7dc07q0my4wd5401968a0nqj030a9vgng2";
    };
    # (mbrock backported) fix for usb-related segmentation faults on darwin
    propagatedBuildInputs =
      stdenv.lib.optionals stdenv.isDarwin
        (with self.pkgs; [ darwin.libobjc darwin.apple_sdk.frameworks.IOKit ]);
  });

  pandoc-tangle =
    self.pkgs.haskell.lib.justStaticExecutables
      (haskellPackages.callPackage ./pkgs/pandoc-tangle.nix {});
}
