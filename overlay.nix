self: super:

let
  lib = self.pkgs.lib;
  stdenv = self.pkgs.stdenv;

in rec {
  dapptoolsSrc = self.callPackage (import ./nix/dapptools-src.nix) {};

  haskellPackages =
    super.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (
      import ./haskell.nix { inherit lib; pkgs = self;}
    );
  });

  unwrappedHaskellPackages =
    super.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (
      import ./haskell.nix { inherit lib; pkgs = self; wrapped = false;}
    );
  });

  sharedHaskellPackages =
    super.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: {})) (
      import ./haskell.nix { inherit lib; pkgs = self; wrapped = false; shared = true; }
    );
  });

  solidityPackage = import ./nix/solidity-package.nix {
    inherit (self) pkgs;
  };

  # experimental dapp builder, allows for easy overriding of phases
  buildDappPackage = import ./nix/build-dapp-package.nix { inherit (self) pkgs; };

  # Here we can make e.g. integration tests for Dappsys.
  dapp-tests = import ./src/dapp-tests { inherit (self) pkgs; };

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
        }) { inherit (super) system; };
      in
        self.pkgs.recurseIntoAttrs (
          fetchSolcVersions { owner = "NixOS";   attr = super.system; }
          //
          fetchSolcVersions { owner = "dapphub"; attr = "unreleased_" + super.system; }
        );

  solc = self.pkgs.runCommand "solc" { } ''
    mkdir -p $out/bin
    ln -s ${solc-static-versions.solc_0_8_6}/bin/solc-0.8.6 $out/bin/solc
  '';

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

  eth-utils = self.pkgs.haskell.lib.justStaticExecutables self.haskellPackages.eth-utils;

  libff = self.callPackage (import ./nix/libff.nix) {};

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

  # Needed for --nix-run subcommands to work,
  # see `nix help run` for more info.
  go-ethereum = super.go-ethereum.overrideAttrs (geth: {
    meta = geth.meta // { mainProgram = "geth"; };
  });

  # We use this to run private testnets without
  # the pesky transaction size limit.
  go-ethereum-unlimited = super.go-ethereum.overrideAttrs (geth: {
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
        'GasLimit:   11500000,' \
        'GasLimit:   0xffffffffffffffff,'

      substituteInPlace params/version.go --replace stable unlimited
    '';
    # Needed for --nix-run subcommands to work,
    # see `nix help run` for more info.
    meta = geth.meta // { mainProgram = "geth"; };
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
