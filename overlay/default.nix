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

in rec {
  solc = callPackage ((import ./solc-versions.nix).solc_0_4_16) {};

  solc-versions =
    super.lib.mapAttrs
      (_: value: pastPackage value {})
      (import ./solc-versions.nix);

  python3 = python36;
  python36 = super.python36.override {
    packageOverrides = (import ./python.nix { pkgs = super.pkgs; });
  };

  hsevm =
    self.pkgs.haskell.lib.justStaticExecutables
      (self.pkgs.haskellPackages.callPackage ./pkgs/hsevm.nix {});

  seth   = callPackage ./pkgs/seth.nix {};
  dapp   = callPackage ./pkgs/dapp.nix {};
  setzer = callPackage ./pkgs/setzer.nix {};
  keeper = callPackage ./pkgs/keeper.nix {};

  go-ethereum = super.go-ethereum.overrideDerivation (_: rec {
    name = "go-ethereum-${version}";
    version = "1.7.0";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "0ybjaiyrfb320rab6a5r9iiqvkrcd8b2qvixzx0kjmc4a7l1q5zh";
    };
  });
}
