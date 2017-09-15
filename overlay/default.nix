self: super:

let
  callPackage = self.pkgs.callPackage;

in rec {
  solc = solc-versions.solc_0_4_16;

  solc-versions =
    super.lib.mapAttrs
      (_: value: super.pkgs.callPackage value {})
      (import ./solc-versions.nix);

  python3 = python36;
  python36 = super.python36.override {
    packageOverrides = (import ./python.nix { pkgs = super.pkgs; });
  };

  hsevm =
    self.pkgs.haskell.lib.justStaticExecutables
      (self.pkgs.haskellPackages.callPackage ./pkgs/hsevm.nix {});

  seth = callPackage ./pkgs/seth.nix {};
  dapp = callPackage ./pkgs/dapp.nix {};
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
