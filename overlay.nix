self: super:

let
  # This is a specific revision of Nixpkgs that we use to avoid
  # rebuilding all the versions of solc when we bump our submodule, or
  # to allow a package to succeed when something breaks in nixpkgs.
  version = "18.09";

  lib = self.pkgs.lib;
  stdenv = self.pkgs.stdenv;

in rec {
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

  solidityPackage = import ./nix/solidity-package.nix {
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

  known-contracts = import ./nix/known-contracts.nix;
  dapp-which = self.callPackage ./nix/dapp-which.nix {};

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
    test-hevm = import ./nix/dapp/dapp-test-hevm.nix { pkgs = self.pkgs; };
  };

  solc-versions =
    super.lib.mapAttrs
      (_: value: self.callPackage value {})
      (import ./nix/solc/versions.nix);
  solc = solc-versions.solc_0_5_3;

  hevm = self.pkgs.haskell.lib.justStaticExecutables self.haskellPackages.hevm;

  jays = (
    self.pkgs.haskell.lib.justStaticExecutables
      (self.haskellPackages.callPackage (import ./src/jays) {})
  ).overrideAttrs (_: { postInstall = "cp $out/bin/{jays,jshon}"; });

  # Override buggy jshon program with Haskell-based replacement.
  jshon = self.jays;

  seth = self.callPackage (import ./src/seth) {};
  dapp = self.callPackage (import ./src/dapp) {};

  ethsign = (self.callPackage (import ./src/ethsign) {}).bin;

  evmdis = self.callPackage ./nix/evmdis.nix {};

  token = self.callPackage (import ./src/token) {};
  dai = self.callPackage (import ./submodules/dai-cli) {};
  mcd = self.callPackage (import ./submodules/mcd-cli) {};

  setzer = self.callPackage (import ./submodules/setzer) {};
  terra = self.callPackage (import ./submodules/terra) {};
  chief = self.callPackage (import ./submodules/chief) {};

  go-ethereum = (super.go-ethereum.overrideDerivation (_: rec {
    name = "go-ethereum-${version}";
    version = "1.8.22";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "0ag9qxrf7n0qkccaf6v4jaysivpxvsy5zfzar3mcm65223pqy375";
    };
    # (mbrock backported) fix for usb-related segmentation faults on darwin
    propagatedBuildInputs =
      stdenv.lib.optionals stdenv.isDarwin
        (with self.pkgs; [ darwin.libobjc darwin.apple_sdk.frameworks.IOKit ]);
  })).override { buildGoPackage = super.buildGo19Package; };

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
  ethabi = ((import ./nix/ethabi { pkgs = super; }).ethabi_cli_4_0_0);

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

  celf = self.callPackage ./nix/celf.nix {};

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

}
