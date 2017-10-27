self: super:

let

  versions = self.lib.importJSON ./versions.json;
  versioned = v: pkg: pkg.overrideAttrs (_: {
    src = self.pkgs.fetchFromGitHub v.src;
  });

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

  lib = self.pkgs.lib;
  stdenv = self.pkgs.stdenv;

  haskellPackages = super.pkgs.haskellPackages.override {
    overrides = _: super-hs: {
      restless-git = versioned versions.restless-git (
        self.pkgs.haskell.lib.dontCheck
          (super-hs.callPackage ./upstream/restless-git.nix {})
      );
    };
  };

  # profilingHaskellPackages = haskellPackages.override {
  #   overrides = self: super-hs:
  #     (import ./haskell.nix { pkgs = super.pkgs; } self super-hs) // {
  #       mkDerivation = args: super-hs.mkDerivation
  #         (args // { enableLibraryProfiling = true; });
  #     };
  # };

in rec {
  solc = callPackage ((import ./solc-versions.nix).solc_0_4_18) {};

  solc-versions =
    super.lib.mapAttrs
      (_: value: pastPackage value {})
      (import ./solc-versions.nix);

  python3 = python36;
  python36 = super.python36.override {
    packageOverrides = (import ./python.nix { pkgs = super.pkgs; });
  };

  hevm = (
    versioned versions.hevm (
      self.pkgs.haskell.lib.justStaticExecutables
        (haskellPackages.callPackage ./upstream/hevm.nix {})
    )
  ).overrideAttrs (attrs: {
    postInstall = ''
      wrapProgram $out/bin/hevm \
         --suffix PATH : "${lib.makeBinPath (with self.pkgs; [bash coreutils git])}"
    '';

    enableSeparateDataOutput = true;
    buildInputs = attrs.buildInputs ++ [self.pkgs.solc];
    nativeBuildInputs = attrs.nativeBuildInputs ++ [self.pkgs.makeWrapper];
  });

  jays = (
    versioned versions.jays (
      self.pkgs.haskell.lib.justStaticExecutables
        (haskellPackages.callPackage ./upstream/jays.nix {})
    )
  ).overrideAttrs (_: { postInstall = "cp $out/bin/{jays,jshon}"; });

  # Override buggy jshon program with Haskell-based replacement.
  jshon = jays;

  seth = versioned versions.seth (callPackage ./upstream/seth.nix {});
  dapp = versioned versions.dapp (callPackage ./upstream/dapp.nix {});

  setzer = callPackage ./setzer.nix {};
  keeper = callPackage ./keeper.nix {};

  go-ethereum = super.go-ethereum.overrideDerivation (_: rec {
    name = "go-ethereum-${version}";
    version = "1.7.2";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "11n77zlf8qixhx26sqf33v911716msi6h0z4ng8gxhzhznrn2nrd";
    };
    # (mbrock backported) fix for usb-related segmentation faults on darwin
    propagatedBuildInputs =
      stdenv.lib.optionals stdenv.isDarwin
        (with self.pkgs; [ darwin.libobjc darwin.apple_sdk.frameworks.IOKit ]);
  });

  # We use this to run private testnets without
  # the pesky transaction size limit.
  go-ethereum-unlimited = go-ethereum.overrideDerivation (this: rec {
    name = "go-ethereum-unlimited-${this.version}";
    preConfigure = ''
      substituteInPlace core/tx_pool.go --replace 'return ErrOversizedData' ""
      substituteInPlace params/protocol_params.go --replace \
        'MaxCodeSize = 24576' \
        'MaxCodeSize = 1000000'
    '';
  });
}
