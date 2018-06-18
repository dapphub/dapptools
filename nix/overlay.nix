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

  lib = self.pkgs.lib;
  stdenv = self.pkgs.stdenv;

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (
      import ./haskell.nix { inherit lib; pkgs = self; }
    );
  });

  profilingHaskellPackages = self.haskellPackages.extend (
    self: super-hs: {
      mkDerivation = args: super-hs.mkDerivation
        (args // { enableLibraryProfiling = true; });
    }
  );

  callSolidityPackage = self.lib.callPackageWith {
    inherit (self) solidityPackage dappsys;
  };

  dappsys = self.callPackage (
    self.pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "dappsys";
      rev = "73dea5a7d1e265dd2921ba420efbfcca3e8cdcc8";
      sha256 = "16hnlim0da8sh7l3rhd6lxdxhhaskbkabr8zf9mx6s5vahyc39gl";
      fetchSubmodules = true;
    }
  ) {};

  solidityPackage = import ./solidity-package.nix {
    inherit (self) pkgs;
  };

  dapps = {
    maker-otc = import (self.pkgs.fetchFromGitHub {
      owner = "makerdao";
      repo = "maker-otc";
      rev = "513f102ad20129ea76e5c9b79afaa18693f63b88";
      sha256 = "0jpdanhihv94yw3ay8dfcbv7l1dg30rfbdxq9lshm0hg94mblb6l";
    }) self.pkgs;
  };

  known-contracts = import ./known-contracts.nix;
  dapp-which = callPackage ./packages/dapp-which.nix {};

  bashScript = { name, version ? "0", deps ? [], text, check ? true } :
    self.pkgs.writeTextFile {
      name = "${name}-${version}";
      executable = true;
      destination = "/bin/${name}";
      text = ''
        #!${self.pkgs.bash}/bin/bash
        set -euo pipefail
        shopt -s lastpipe
        export PATH="${lib.makeBinPath deps}:/run/wrappers/bin"
        ${text}
      '';
      checkPhase = ''
        ${self.pkgs.bash}/bin/bash -n $out/bin/${name}
      '' + (if check then ''
        ${self.pkgs.shellcheck}/bin/shellcheck -s bash $out/bin/${name}
      '' else "");
    };

  dapp2 = {
    test-hevm = import ./dapp/dapp-test-hevm.nix { pkgs = self.pkgs; };
  };

  solc = callPackage ((import ./packages/solc-versions.nix).solc_0_4_23) {};
  solc-versions =
    super.lib.mapAttrs
      (_: value: pastPackage value {})
      (import ./packages/solc-versions.nix);

  python3 = self.python36;
  python36 = super.python36.override {
    packageOverrides = (import ./packages/python.nix { pkgs = super.pkgs; });
  };

  symbex =
    self.pkgs.haskell.lib.justStaticExecutables
      (self.haskellPackages.callPackage (import ../symbex) {});

  # hevm = self.pkgs.haskell.lib.justStaticExecutables self.haskellPackages.hevm;
  hevm = self.haskellPackages.hevm;

  jays = (
    self.pkgs.haskell.lib.justStaticExecutables
      (self.haskellPackages.callPackage (import ../jays) {})
  ).overrideAttrs (_: { postInstall = "cp $out/bin/{jays,jshon}"; });

  # Override buggy jshon program with Haskell-based replacement.
  jshon = self.jays;

  seth = callPackage (import ../seth) {};
  dapp = callPackage (import ../dapp) {};

  ethsign = (callPackage (import ../ethsign) {}).bin;

  setzer = callPackage (import ../setzer) {};

  keeper = callPackage ./packages/keeper.nix {};
  evmdis = callPackage ./packages/evmdis.nix {};

  token = callPackage (import ../token) {};
  dai = callPackage (import ../dai-cli) {};

  go-ethereum = super.go-ethereum.overrideDerivation (_: rec {
    name = "go-ethereum-${version}";
    version = "1.8.10";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "1n36pz4y3xa4d46mynym98bra79qx5n9lb29chyxfpvi5fmprdg1";
    };
    # (mbrock backported) fix for usb-related segmentation faults on darwin
    propagatedBuildInputs =
      stdenv.lib.optionals stdenv.isDarwin
        (with self.pkgs; [ darwin.libobjc darwin.apple_sdk.frameworks.IOKit ]);
  });

  # We use this to run private testnets without
  # the pesky transaction size limit.
  go-ethereum-unlimited = self.go-ethereum.overrideDerivation (this: rec {
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
  ethabi = ((import ./packages/ethabi { pkgs = super; }).ethabi_cli_4_0_0);

  qrtx = self.bashScript {
    name = "qrtx";
    version = "0";
    deps = with self.pkgs; [qrencode feh vim gnused coreutils];
    text = ''
      sed 's/^0x//' | tr -d '[:space:]' | xxd -r -p | base64 -w0 |
        qrencode -s 1 -o - | feh -ZB white --force-aliasing -
    '';
  };

  qrtx-term = self.bashScript {
    name = "qrtx-term";
    version = "0";
    deps = with self.pkgs; [qrencode vim gnused coreutils];
    text = ''
      sed 's/^0x//' | tr -d '[:space:]' | xxd -r -p | base64 -w0 |
        qrencode -t ANSIUTF8
    '';
  };

  secp256k1 = super.secp256k1.overrideDerivation (_: {
    dontDisableStatic = true;
  });

  ethjet = callPackage (import ../libethjet) {};
  
  mkbip39 = with self.pkgs.python3Packages; buildPythonApplication rec {
    version = "0.5";
    name = "mkbip39";
    src = ./packages/mkbip39;
    propagatedBuildInputs = [mnemonic];
  };

  celf = callPackage ./packages/celf.nix {};

  myetherwallet = stdenv.mkDerivation rec {
    name = "myetherwallet-${version}";
    version = "3.11.3.1";
    src = self.fetchFromGitHub {
      owner = "kvhnuke";
      repo = "etherwallet";
      rev = "v${version}";
      sha256 = "1985zhy8lwnyg5hc436gcma0z9azm1qzsl3rj2vqq080s5czm4d2";
    };
    installPhase = ''
      mkdir -p $out/myetherwallet
      cp -R dist/* $out/myetherwallet
    '';
  };

  dafny = super.dafny.overrideAttrs (_: rec {
    name = "Dafny-${version}";
    version = "2.1.0";

    src = self.fetchurl {
      url = "https://github.com/Microsoft/dafny/archive/v${version}.tar.gz";
      sha256 = "1iyhy0zpi6wvqif7826anzgdipgsy5bk775ds9qqwfw27j7x6fy5";
    };

    postPatch = ''
      sed -i \
        -e 's/ Visible="False"//' \
        -e "s/Exists(\$(CodeContractsInstallDir))/Exists('\$(CodeContractsInstallDir)')/" \
        Source/*/*.csproj
    '';
  });

} // (import ./weird.nix self super)
