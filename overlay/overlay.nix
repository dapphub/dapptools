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

  ourPerlPackages = import ./perl.nix {
    inherit (self) buildPerlPackage perlPackages;
  };

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

  callSolidityPackage = self.lib.callPackageWith {
    inherit solidityPackage;
    inherit dappsys;
  };

  dappsys = self.callPackage (
    self.pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "dappsys-nix";
      rev = "1c51729ea24f0041a03722c81ccef75bce2eb9cc";
      sha256 = "1ll0r7k30hgr8pkmymdvm5lhpzpi9vshzxc2r5wrxra6kvjbhs7b";
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

  known-contracts = import ../known-contracts.nix;
  dapp-which = callPackage ./dapp-which.nix {};

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

  dapphub-emacs-experiment = let
    version = "1";
    dapphub-elisp = self.pkgs.writeTextFile {
      name = "dapphub-el-${version}";
      destination = "/dapphub.el";
      text = ''
        (package-initialize)
        (load-theme 'solarized-dark t)
        (set-face-attribute 'default (selected-frame) :height 180)
        (menu-bar-mode -1)
        (tool-bar-mode -1)
        (setq initial-buffer-choice
              (lambda ()
                (with-current-buffer (get-buffer-create "*DappHub*")
                  (insert ";; Hello, and welcome to DappHub!")
                  (current-buffer))))
      '';
    };
  in bashScript {
    inherit version;
    name = "dapphub-emacs-experiment";
    deps = with self.pkgs; [
      coreutils
      (emacsWithPackages (e: with e; [
        solarized-theme
      ]))
      ethsign
      seth
    ];
    text = ''
      emacs -q --no-splash --load=${dapphub-elisp}/dapphub.el
    '';
  };

  celf = callPackage ./celf.nix {};

  # shellcheck = super.shellcheck.overrideDerivation (_: rec {
  #   name = "shellcheck-${version}";
  #   version = "0.4.7";
  #   src = self.pkgs.fetchFromGitHub {
  #     owner = "koalaman";
  #     repo = "shellcheck";
  #     rev = "v${version}";
  #     sha256 = "08j33ipk7w56vj315smq9hxz512gbi5w283d7mvcyjvpddr001gc";
  #   };
  # });

  # Use an older version of PolyML, for TLA+.
  # I'm not familiar with these tools, but this seemed to be necessary.
  # The specific commit is the parent of a change that is incompatible
  # with some stage of the TLA+ build.
  polyml = self.polyml56.overrideDerivation (_: rec {
    name = "polyml-${version}";
    version = "unstable-2015-10-15";
    src = self.pkgs.fetchFromGitHub {
      owner = "polyml";
      repo = "polyml";
      rev = "257ef837c10e685170909878a64339b1144ff960";
      sha256 = "00migdfxzj2m28ikbnpbri8aysf5w836qdmflmrpxk7mddncimvw";
    };
  });

  tla-plus =
    let
      core = callPackage ./tla/core.nix {};
      toolbox = callPackage ./tla/toolbox.nix { gtk = self.gtk2; };
      isabelle2011-1 =
        callPackage ./tla/isabelle2011-1 {
          proofgeneral = self.emacsPackages.proofgeneral;
        };
      tlaps = callPackage ./tla/tlaps.nix {
        inherit isabelle2011-1;
      };
      tla-smt = with self; [z3 yices cvc3];
  in super.buildEnv {
    name = "tla-plus-full";
    paths = [toolbox] ++ core.all ++ tlaps.all ++ tla-smt;
  };

  git-stitch-repo = ourPerlPackages.GitFastExport;

  tomono = stdenv.mkDerivation rec {
    name = "tomono-${version}";
    version = "unstable-2018-01-03";
    src = self.fetchFromGitHub {
      owner = "unravelin";
      repo = "tomono";
      rev = "ec59cb019a181f461e769feb22d43e09cf907566";
      sha256 = "1s2bl8iwwalslh46gp37zrg19jvbzb65sajrqkhwb3bkbbx4s9pd";
    };
    installPhase = ''
      mkdir -p $out/bin
      cp tomono.sh $out/bin/tomono
    '';
    postInstall = ''
      wrapProgram $out/bin/tomono \
        --suffix PATH : "${lib.makeBinPath (with self.pkgs; [bash coreutils git])}"
    '';
    nativeBuildInputs = [self.pkgs.makeWrapper];
  };

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
}
