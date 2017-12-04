{ flavor ? "stable", self, super }:

let

  versions = self.lib.importJSON ./versions.json;
  versioned = pkg: caller: (caller (import (./upstream + "/${flavor}/${pkg}.nix"))).overrideAttrs (x: {
    src = self.pkgs.fetchFromGitHub versions.${pkg}.${flavor};
  } // (if flavor == "stable" then rec {
    name = "${pkg}-${version}";
    version = versions.${pkg}.version;
  } else {
    name = "${pkg}-${flavor}";
    version = flavor;
  }));

  local = src: pkg: pkg.overrideAttrs (_: {
    inherit src;
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
    overrides = _: super-hs: let
      dontCheck = x: self.pkgs.haskell.lib.dontCheck (super-hs.callPackage x {});
    in {
      restless-git = versioned "restless-git" dontCheck;
      symbex = versioned "symbex" dontCheck;

      # We don't want Megaparsec 5!
      megaparsec = super.pkgs.haskellPackages.megaparsec_6_2_0;
    };
  };

  profilingHaskellPackages = haskellPackages.override {
    overrides = self: super-hs: {
      mkDerivation = args: super-hs.mkDerivation
        (args // { enableLibraryProfiling = true; });
    };
  };

in rec {
  dappsys = import ../dappsys {
    inherit (self.pkgs) dappsys solidityPackage;
  };

  solidityPackage = import ./solidity-package.nix {
    inherit (self) pkgs;
  };

  bashScript = { name, deps ? [], text } :
    self.pkgs.writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";
      text = ''
        #!${self.pkgs.bash}/bin/bash
        set -e
        export PATH="${lib.makeBinPath deps}"
        ${text}
      '';
      checkPhase = ''
        ${self.pkgs.bash}/bin/bash -n $out/bin/${name}
        ${self.pkgs.shellcheck}/bin/shellcheck $out/bin/${name}
      '';
    };

  dapp2 = {
    test-hevm = import ./dapp/dapp-test-hevm.nix { pkgs = self.pkgs; };
  };

  solc = callPackage ((import ./solc-versions.nix).solc_0_4_19) {};
  solc-versions =
    super.lib.mapAttrs
      (_: value: pastPackage value {})
      (import ./solc-versions.nix);

  python3 = python36;
  python36 = super.python36.override {
    packageOverrides = (import ./python.nix { pkgs = super.pkgs; });
  };

  symbex-mueval = let
    env = haskellPackages.ghcWithPackages (self: with self; [
      symbex QuickCheck show simple-reflect
    ]);
  in (haskellPackages.mueval.override {
    hint = haskellPackages.hint.override {
      ghc = env;
    };
  }).overrideAttrs (attrs: {
    preConfigure = ''
      substituteInPlace Mueval/ArgsParse.hs \
        --replace 'Just defaultModules' 'Just []'
    '';
    postInstall = ''
      wrapProgram $out/bin/mueval \
        --set NIX_GHC ${env}/bin/ghc \
        --set NIX_GHCPKG ${env}/bin/ghc-pkg \
        --set NIX_GHC_LIBDIR $(${env}/bin/ghc --print-libdir)
    '';
    nativeBuildInputs = attrs.nativeBuildInputs ++ [self.pkgs.makeWrapper];
  });

  hevmas = self.pkgs.bashScript {
    name = "hevmas";
    deps = with self.pkgs; [symbex-mueval gnused];
    text = ''
      mueval -XRecursiveDo -m EVM.Assembly \
        -e "$(echo "bytecode $ mdo"; sed 's/^/  /')" \
        | sed -e 's/^"//' -e 's/"$//'
    '';
  };

  hevml = self.pkgs.bashScript {
    name = "hevml";
    deps = with self.pkgs; [
      coreutils
      (haskellPackages.ghcWithPackages (x: with x; [symbex]))
    ];
    text = ''
      { echo "import qualified Prelude"
        cat
        echo
        echo "main :: Prelude.IO ()"
        echo "main = Prelude.putStrLn (bytecode contract)"
      } | runghc --ghc-arg=-XNoImplicitPrelude --ghc-arg=-XRecursiveDo
    '';
  };

  hevm = (
    versioned "hevm"
      (x: self.pkgs.haskell.lib.justStaticExecutables (haskellPackages.callPackage x {}))
  ).overrideAttrs (attrs: {
    postInstall = ''
      wrapProgram $out/bin/hevm \
         --suffix PATH : "${lib.makeBinPath (with self.pkgs; [bash coreutils git])}"
    '';

    enableSeparateDataOutput = true;
    buildInputs = attrs.buildInputs ++ [self.pkgs.solc];
    nativeBuildInputs = attrs.nativeBuildInputs ++ [self.pkgs.makeWrapper];
  });

  hevm-prof = (
    versioned "hevm"
      (x: self.pkgs.haskell.lib.justStaticExecutables (profilingHaskellPackages.callPackage x {}))
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
    versioned "jays" (x:
      self.pkgs.haskell.lib.justStaticExecutables
        (haskellPackages.callPackage x {})
    )
  ).overrideAttrs (_: { postInstall = "cp $out/bin/{jays,jshon}"; });

  # Override buggy jshon program with Haskell-based replacement.
  jshon = jays;

  seth = versioned "seth" (x: callPackage x {});
  dapp = versioned "dapp" (x: callPackage x {});

  ethsign = versioned "ethsign" (x: (callPackage x {}).bin);

  dapp-prof = (
    (versioned "dapp" (x: callPackage x {})).override { hevm = hevm-prof; }
  ).overrideAttrs (_: {
    preConfigure = ''
      substituteInPlace libexec/dapp/dapp-test-hevm \
        --replace 'hevm ' 'hevm +RTS -xc -RTS '
    '';
  });


  dappsys-legacy = (import ./dappsys.nix { inherit (self) pkgs; }).dappsys;

  setzer = versioned "setzer" (x: callPackage x {});

  keeper = callPackage ./keeper.nix {};
  evmdis = callPackage ./evmdis.nix {};

  go-ethereum = super.go-ethereum.overrideDerivation (_: rec {
    name = "go-ethereum-${version}";
    version = "1.7.3";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "1w6rbq2qpjyf2v9mr18yiv2af1h2sgyvgrdk4bd8ixgl3qcd5b11";
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
      # Huge transaction calldata
      substituteInPlace core/tx_pool.go --replace 'return ErrOversizedData' ""

      # Huge contracts
      substituteInPlace params/protocol_params.go --replace \
        'MaxCodeSize = 24576' \
        'MaxCodeSize = 1000000'

      # Huge block gas limit in --dev mode
      substituteInPlace core/genesis.go --replace \
        'GasLimit:   6283185,' \
        'GasLimit:   0xffffffffffffffff,'
    '';
  });

  # Use unreleased ethabi that fixes empty array encoding.
  ethabi = ((import ./ethabi { pkgs = super; }).ethabi_cli_4_0_0);
}
