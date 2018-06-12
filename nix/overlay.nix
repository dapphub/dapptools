{ flavor ? "stable" }: self: super:

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

  makeIso = { module, config }:
    self.pkgs.lib.hydraJob (
      (import ../nixpkgs/nixos/lib/eval-config.nix {
        inherit (self) pkgs;
        system = "x86_64-linux";
        modules = [module config];
      }).config.system.build.isoImage
    );

  ourPerlPackages = import ./packages/perl.nix {
    inherit (self) buildPerlPackage perlPackages;
  };

in {
  haskellPackages = super.haskellPackages.extend (
    self-hs: super-hs:
      let
        dontCheck = x:
          self.haskell.lib.dontCheck
            (self-hs.callPackage x {});
      in {
        restless-git = dontCheck (import ../restless-git);
        symbex = dontCheck (import ../symbex);
        ethjet = self-hs.callPackage (import ../libethjet-haskell) {
          # Haskell libs with the same names as C libs...
          # Depend on the C libs, not the Haskell libs.
          # These are system deps, not Cabal deps.
          inherit (self.pkgs) secp256k1 ethjet;
        };

        # We don't want Megaparsec 5!
        megaparsec = self-hs.megaparsec_6_2_0;

        hevm = (
          self-hs.callPackage (import ../hevm) {}
        ).overrideAttrs (attrs: {
          postInstall = ''
            wrapProgram $out/bin/hevm \
               --suffix PATH : "${lib.makeBinPath (with self.pkgs; [bash coreutils git])}"
          '';

          enableSeparateDataOutput = true;
          buildInputs = attrs.buildInputs ++ [self.pkgs.solc];
          nativeBuildInputs = attrs.nativeBuildInputs ++ [self.pkgs.makeWrapper];
        });
      }
    );

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

  symbex-mueval = let
    env = self.haskellPackages.ghcWithPackages (x: with x; [
      symbex QuickCheck show simple-reflect
    ]);
  in (self.haskellPackages.mueval.override {
    hint = self.haskellPackages.hint.override {
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
      (haskellPackages.ghcWithPackages (x: [x.symbex]))
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

  hevmls = self.pkgs.bashScript {
    name = "hevmls";
    version = "0";
    deps = with self.pkgs; [
      coreutils
      (haskellPackages.ghcWithPackages (x: [x.symbex]))
    ];
    text = ''
      { echo "import qualified Prelude"
        echo "import qualified EVM.Symbex.Main as Symbex"
        cat
        echo
        echo "main :: Prelude.IO ()"
        echo "main = Symbex.showPaths (Symbex.run (assemble contract))"
      } | runghc --ghc-arg=-XNoImplicitPrelude --ghc-arg=-XRecursiveDo
    '';
  };

  hevmlsj = self.pkgs.bashScript {
    name = "hevmlsj";
    version = "0";
    deps = with self.pkgs; [
      coreutils
      (haskellPackages.ghcWithPackages (x: [x.symbex]))
    ];
    text = ''
      { echo "import qualified Prelude"
        echo "import qualified EVM.Symbex.Main as Symbex"
        echo "import qualified EVM.Symbex as Symbex"
        echo "import qualified Data.ByteString.Lazy.Char8 as B8"
        echo "import qualified Data.Aeson as Aeson"
        echo "import Prelude ((.), ($))"
        cat
        echo
        echo "main :: Prelude.IO ()"
        echo "main = B8.putStrLn . Aeson.encode $"
        echo "  Symbex.step' (assemble contract) Symbex.emptyState"
      } | runghc --ghc-arg=-XNoImplicitPrelude --ghc-arg=-XRecursiveDo
    '';
  };

  symbex =
    self.pkgs.haskell.lib.justStaticExecutables
      (self.haskellPackages.callPackage (import ../symbex) {});

  hevm = self.pkgs.haskell.lib.justStaticExecutables self.haskellPackages.hevm;

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

  dialog-to-file = self.bashScript {
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

  eth-pick-account = self.bashScript {
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
    vote = self.bashScript {
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
    module = import ./ethos.nix { hidpi = false; };
    config.isoImage.appendToMenuLabel = " (Ethos by DappHub)";
  };

  ethos-iso-hidpi = makeIso {
    module = import ./ethos.nix { hidpi = true; };
    config.isoImage.appendToMenuLabel = " (Ethos by DappHub, HiDPI)";
  };

  mkbip39 = with self.pkgs.python3Packages; buildPythonApplication rec {
    version = "0.5";
    name = "mkbip39";
    src = ./packages/mkbip39;
    propagatedBuildInputs = [mnemonic];
  };

  # oasis-orders = (
  #   versioned "oasis-orders"
  #     (x: self.pkgs.haskell.lib.justStaticExecutables
  #       (self.haskellPackages.callPackage x {}))
  # ).overrideAttrs (attrs: {
  #   postInstall = ''
  #     wrapProgram $out/bin/oasis-orders \
  #       --set OASIS_DAPP_PATH ${self.dapps.maker-otc}/dapp/maker-otc
  #   '';
  #   nativeBuildInputs = attrs.nativeBuildInputs ++ [self.pkgs.makeWrapper];
  # });

  dapphub-emacs-experiment = let
    version = "1";
    dapphub-elisp = self.pkgs.writeTextFile {
      name = "dapphub-emacs-${version}";
      destination = "/dapphub.el";
      text = ''
        (package-initialize)
        (set-face-attribute 'default (selected-frame) :height 180 :family "Courier")
        (menu-bar-mode -1)
        (tool-bar-mode -1)
        (setq initial-buffer-choice
              (lambda ()
                (with-current-buffer (get-buffer-create "*DappHub*")
                  (insert ";; Hello, and welcome to DappHub!")
                  (current-buffer))))
        (require 'agda2-mode)
        (setq auto-mode-alist '(("\\.agda" . agda2-mode)))
        (setq agda2-program-args (list (concat "--include-path=" (expand-file-name "~/src/agda-stdlib/src"))))
      '';
    };
  in self.bashScript {
    inherit version;
    name = "dapphub-emacs-experiment";
    deps = with self.pkgs; [
      coreutils
      (emacsWithPackages (e: with e; [
        zenburn-theme agda2-mode
      ]))
      ethsign
      seth
      haskellPackages.Agda
    ];
    text = ''
      emacs -q --no-splash --load=${dapphub-elisp}/dapphub.el
    '';
  };

  celf = callPackage ./packages/celf.nix {};

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
      core = callPackage ./packages/tla/core.nix {};
      toolbox = callPackage ./packages/tla/toolbox.nix { gtk = self.gtk2; };
      isabelle2011-1 =
        callPackage ./packages/tla/isabelle2011-1 {
          proofgeneral = self.emacsPackages.proofgeneral;
        };
      tlaps = callPackage ./packages/tla/tlaps.nix {
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

  ocaml-getopt = stdenv.mkDerivation rec {
    name = "ocaml-getopt-${version}";
    version = "unstable-2012-06-15";
    src = self.fetchurl {
      url = "https://forge.ocamlcore.org/frs/download.php/896/ocaml-getopt-20120615.tar.gz";
      sha256 = "1rz2mi3gddwpd249bvz6h897swiajk4d6cczrsscibwpkmdvrfwa";
    };
    buildInputs = with self.ocamlPackages; [ocaml ocamlbuild findlib];
    createFindlibDestdir = true;
  };

  dry-analyzer = stdenv.mkDerivation rec {
    name = "dry-analyzer-${version}";
    version = "unstable-2017-07-10";
    src = ~/src/dry-analyzer;
    buildInputs = with self.ocamlPackages; [
      ocaml cohttp cohttp-lwt cohttp-lwt-unix batteries ocamlnet ocamlbuild findlib ocaml-getopt
    ] ++ [self.pkgs.coq];
    patchPhase = ''
      patchShebangs ./compile.sh
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
