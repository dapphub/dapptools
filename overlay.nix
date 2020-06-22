self: super:

let
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
      rev = "933fb468f72f1a81ee8b6d90b17ffdf25d8e1067";
      sha256 = "06k5j597y4i1q6zcpqxzbflzq6qp62nrnjs6slh7vk70wjccrbh9";
      fetchSubmodules = true;
    }
  ) {};

  solidityPackage = import ./nix/solidity-package.nix {
    inherit (self) pkgs;
  };

  # experimental dapp builder, allows for easy overriding of phases
  buildDappPackage = import ./nix/build-dapp-package.nix { inherit (self) pkgs; };

  # A merged Dappsys to act as the DAPPSYS_PATH for dapp-tests.
  dappsys-merged = self.symlinkJoin {
    name = "dappsys";
    paths = map builtins.toString (lib.attrVals [
      "ds-test"
      "ds-auth"
      "ds-math"
    ] dappsys);
  };

  # Here we can make e.g. integration tests for Dappsys,
  # or tests that verify Hevm correctness, etc.
  dapp-tests = import ./src/dapp-tests { inherit (self) pkgs; };

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
    let
      fetchSolcVersions = { owner, attr }:
        super.lib.mapAttrs
          (_: nixpkgs: (importNixpkgs { inherit owner; inherit (nixpkgs) rev sha256; }).solc)
          (builtins.getAttr attr (import ./nix/solc-versions.nix));
      importNixpkgs = { owner, rev, sha256 }:
        import (self.pkgs.fetchFromGitHub {
          inherit owner rev sha256;
          repo = "nixpkgs";
        }) {};
      in
        fetchSolcVersions { owner = "NixOS";   attr = super.system; }
        //
        fetchSolcVersions { owner = "dapphub"; attr = "unreleased"; };
  solc = solc-versions.solc_0_5_15;

  hevm = self.pkgs.haskell.lib.justStaticExecutables self.haskellPackages.hevm;

  libff = self.callPackage (import ./nix/libff.nix) {};

  cvc4 = self.callPackage (import ./nix/cvc4.nix) {};

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

  setzer = self.callPackage (import ./submodules/setzer) {};
  terra = self.callPackage (import ./submodules/terra) {};
  chief = self.callPackage (import ./submodules/chief) {};

  # We use this to run private testnets without
  # the pesky transaction size limit.
  go-ethereum-unlimited = super.go-ethereum.overrideAttrs (geth: rec {
    name = "${geth.pname}-unlimited-${geth.version}";
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
