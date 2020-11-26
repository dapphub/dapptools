self: super:

let
  lib = self.pkgs.lib;
  stdenv = self.pkgs.stdenv;

in rec {
  dapptoolsSrc = self.callPackage (import ./nix/dapptools-src.nix) {};

  haskellPackages = super.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (
      import ./haskell.nix { inherit lib; pkgs = self; }
    );
  });

  solidityPackage = import ./nix/solidity-package.nix {
    inherit (self) pkgs;
  };

  # experimental dapp builder, allows for easy overriding of phases
  buildDappPackage = import ./nix/build-dapp-package.nix { inherit (self) pkgs; };

  # Here we can make e.g. integration tests for Dappsys.
  dapp-tests = import ./src/dapp-tests { inherit (self) pkgs; };

  # These are tests that verify the correctness of hevm symbolic using various
  # external test suites (e.g. the solc tests)
  hevm-tests = import ./nix/hevm-tests { pkgs = self.pkgs; };

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
        self.pkgs.recurseIntoAttrs (
          fetchSolcVersions { owner = "NixOS";   attr = super.system; }
          //
          fetchSolcVersions { owner = "dapphub"; attr = "unreleased_" + super.system; }
        );
  solc = solc-versions.solc_0_6_7;

  solc-static-versions =
    let
      make-solc-drv = _: solc:
        self.callPackage (
          import ./nix/solc-static.nix {
            path    = solc.path;
            version = solc.version;
            sha256  = solc.sha256;
        }) {};
    in builtins.mapAttrs make-solc-drv
        (builtins.getAttr super.system (import ./nix/solc-static-versions.nix));

  hevm = self.pkgs.haskell.lib.justStaticExecutables self.haskellPackages.hevm;

  libff = self.callPackage (import ./nix/libff.nix) {};

  cvc4 = self.callPackage (import ./nix/cvc4.nix) {};

  z3 = self.callPackage (import ./nix/z3.nix) {};

  jays = (
    self.pkgs.haskell.lib.justStaticExecutables
      (self.haskellPackages.callCabal2nix "jays" (./src/jays) {})
  ).overrideAttrs (_: { postInstall = "cp $out/bin/{jays,jshon}"; });

  # Override buggy jshon program with Haskell-based replacement.
  jshon = self.jays;

  seth = self.callPackage (import ./src/seth) {};
  dapp = self.callPackage (import ./src/dapp) {};

  ethsign = (self.callPackage (import ./src/ethsign) {});

  token = self.callPackage (import ./src/token) {};

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
}
