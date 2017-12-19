{ flavor ? "stable", self, super }:

let

  versions = super.lib.importJSON ./versions.json;
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

  makeIso = { module, config }:
    self.pkgs.lib.hydraJob (
      (import ../nixpkgs/nixos/lib/eval-config.nix {
        inherit (self) pkgs;
        system = "x86_64-linux";
        modules = [module config];
      }).config.system.build.isoImage
    );

in rec {
  haskellPackages = super.haskellPackages.extend (
    _: super-hs: let
      dontCheck = x: super.haskell.lib.dontCheck (super-hs.callPackage x {});
    in {
      restless-git = versioned "restless-git" dontCheck;
      symbex = versioned "symbex" dontCheck;
      ethjet = versioned "libethjet-haskell"
        (x: super-hs.callPackage x {
          # Haskell libs with the same names as C libs...
          # Depend on the C libs, not the Haskell libs.
          # These are system deps, not Cabal deps.
          inherit (self.pkgs) secp256k1 ethjet;
        });

      # We don't want Megaparsec 5!
      megaparsec = super-hs.megaparsec_6_2_0;

      hevm = hevm-lib;
    }
  );

  profilingHaskellPackages = haskellPackages.extend (
    self: super-hs: {
      mkDerivation = args: super-hs.mkDerivation
        (args // { enableLibraryProfiling = true; });
    }
  );

  dappsys = import ../dappsys {
    inherit (self.pkgs) dappsys solidityPackage;
  };

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

  dapp-which = bashScript {
    name = "dapp-which";
    version = "0";
    deps = [self.pkgs.coreutils];
    text = ''
      declare -A table

      table[mkr-2016-03]=0xC66eA802717bFb9833400264Dd12c2bCeAa34a6d
      table[mkr-2017-11]=0x9f8F72aA9304c8B593d555F12eF6589cC3A579A2
      table[weth-2016-06]=0xECF8F87f810EcF450940c9f60066b4a7a501d6A7
      table[weth-2017-12]=0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2
      table[mkr-redeemer-2017-12]=0x642AE78FAfBB8032Da552D619aD43F1D81E4DD7C
      table[oasis-2017-09]=0x3Aa927a97594c3ab7d7bf0d47C71c3877D1DE4A1
      table[oasis-2017-12]=0x14FBCA95be7e99C15Cc2996c6C9d841e54B79425
      table[sai-2017-07]=0x59aDCF176ED2f6788A41B8eA4c4904518e62B6A4
      table[dai-2017-12]=0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359

      if [ "$#" -eq 0 ]; then
        for k in "''${!table[@]}"; do
          echo "$k" "''${table[$k]}"
        done
      elif [ "$#" -eq 1 ]; then
        if [ ''${table[$1]+_} ]; then
          echo "''${table[$1]}"
        else
          for k in "''${!table[@]}"; do
            if [ "''${table[$k],,}" == "''${1,,}" ]; then
              echo "$k"
              exit
            fi
          done
          echo >&2 "dapp-which: don't know $1"
        fi
      else
        echo >&2 "usage: dapp-which [CONTRACT-ID | ADDRESS]"
      fi
    '';
  };

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
        ${self.pkgs.shellcheck}/bin/shellcheck $out/bin/${name}
      '' else "");
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
    version = "0";
    deps = with self.pkgs; [symbex-mueval gnused];
    text = ''
      mueval -XRecursiveDo -m EVM.Assembly \
        -e "$(echo "bytecode $ mdo"; sed 's/^/  /')" \
        | sed -e 's/^"//' -e 's/"$//'
    '';
  };

  hevml = self.pkgs.bashScript {
    name = "hevml";
    version = "0";
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

  hevm = self.pkgs.haskell.lib.justStaticExecutables hevm-lib;

  hevm-lib = (
    versioned "hevm"
      (x: haskellPackages.callPackage x {})
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

  dappsys-legacy = (import ./dappsys.nix { inherit (self) pkgs; }).dappsys;

  setzer = versioned "setzer" (x: callPackage x {});

  keeper = callPackage ./keeper.nix {};
  evmdis = callPackage ./evmdis.nix {};

  token = versioned "token" (x: callPackage x {});
  dai = versioned "dai-cli" (x: callPackage x {});

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

  iosevka-term = let version = "1.13.3"; in self.pkgs.fetchzip rec {
    name = "iosevka-ss02-term-${version}";
    url = "https://github.com/be5invis/Iosevka/releases/download/v${version}/iosevka-term-ss02-${version}.zip";
    sha256 = "03hcmqjbm4rvy1ydar07p7a7vfr2mpfjrr8sbrs5y6z07vdml3xd";
    postFetch = ''
      mkdir -p $out/share/fonts
      unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
    '';
    meta = with stdenv.lib; {
      homepage = https://github.com/be5invis/iosevka;
      description = "Iosevka font (fixed width, Anonymous Pro style)";
      license = licenses.ofl;
      platforms = platforms.all;
    };
  };

  qrtx = bashScript {
    name = "qrtx";
    version = "0";
    deps = with self.pkgs; [qrencode feh vim gnused coreutils];
    text = ''
      sed 's/^0x//' | tr -d '[:space:]' | xxd -r -p | base64 -w0 |
        qrencode -s 1 -o - | feh -ZB white --force-aliasing -
    '';
  };

  qrtx-term = bashScript {
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

  ethjet = versioned "libethjet" (x: callPackage x {});

  dialog-to-file = bashScript {
    name = "dialog-to-file";
    version = "0";
    deps = with self.pkgs; [dialog ncurses];
    text = ''
      set +e
      file=$1; shift
      exec 3>&1; x=$(dialog "$@" 2>&1 1>&3)
      status=$?; exec 3>&-; clear
      if [ "$status" -eq 0 ]; then echo >"$file" "$x"; else exit 1; fi
    '';
  };

  eth-pick-account = bashScript {
    name = "eth-pick-account";
    version = "0";
    deps = with self.pkgs; [ethsign dialog-to-file];
    text = ''
      accts=()
      while read -r x y; do accts+=("$x" "$y"); done < <(ethsign ls 2>/dev/null)
      dialog-to-file "$1" --ok-label "Use account" \
         --menu "Pick an Ethereum account:" \
          20 80 10 "''${accts[@]}"
    '';
  };

  ds-chief = {
    vote = bashScript {
      name = "ds-chief-vote";
      version = "0";
      deps = with self.pkgs; [
        seth eth-pick-account ethsign dialog-to-file ncurses coreutils qrtx readline
      ];
      text = ''
        acctfile=$(mktemp)
        txfile=$(mktemp)
        eth-pick-account "$acctfile"
        acct=$(cat "$acctfile")
        dialog-to-file "$txfile" --ok-label "Sign" \
          --title "Make a signed transaction without publishing" \
          --form "ds-chief vote -- set your approval to an existing slate" 10 60 4 \
           "Slate ID" 1 1 "" 1 12 32 0 \
           "Nonce" 2 1 "1" 2 12 10 0 \
           "Gas price" 3 1 50 3 12 10 0 \
           "Gas limit" 4 1 10000 4 12 10 0 \
           "Account" 5 1 "$acct" 5 12 0 0

        {
          read -r slate
          read -r nonce
          read -r gasprice
          read -r gaslimit
        } < "$txfile"

        slate=$(seth --to-bytes32 "$(seth --from-ascii "$slate")")
        calldata=$(seth calldata 'vote(bytes32)' "$slate")

        echo "Offline ds-chief vote transaction creation"
        echo
        echo "Account:   $acct"
        echo "Slate ID:  $slate"
        echo "Nonce:     $nonce"
        echo "Gas price: $gasprice"
        echo "Gas limit: $gaslimit"
        echo "Call data: $calldata"
        echo
        echo "After authentication, signed transaction will be shown as QR code."

        ethsign tx --from "$acct" --to 0x0 --nonce "$nonce" --gas-price "$gasprice" \
          --gas-limit "$gaslimit" --data "$calldata" \
          --value 0 --chain-id 1 | qrtx
        echo
      '';
    };
  };

  ethos-iso = makeIso {
    module = import ../ethos.nix { hidpi = false; };
    config.isoImage.appendToMenuLabel = " (Ethos by DappHub)";
  };

  ethos-iso-hidpi = makeIso {
    module = import ../ethos.nix { hidpi = true; };
    config.isoImage.appendToMenuLabel = " (Ethos by DappHub, HiDPI)";
  };

  mkbip39 = with self.pkgs.python3Packages; buildPythonApplication rec {
    version = "0.5";
    name = "mkbip39";
    src = ./mkbip39;
    propagatedBuildInputs = [mnemonic];
  };

  oasis-orders = (
    versioned "oasis-orders"
      (x: self.pkgs.haskell.lib.justStaticExecutables
        (haskellPackages.callPackage x {}))
  ).overrideAttrs (attrs: {
    postInstall = ''
      wrapProgram $out/bin/oasis-orders \
        --set OASIS_DAPP_PATH ${dapps.maker-otc}/dapp/maker-otc
    '';
    nativeBuildInputs = attrs.nativeBuildInputs ++ [self.pkgs.makeWrapper];
  });
}
