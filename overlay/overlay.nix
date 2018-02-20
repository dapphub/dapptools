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
      rev = "e449a7767779579d2f5862efc613d8345ba3b2f9";
      sha256 = "030ws18sck7wp4710zcz6hash7v95p2z6mjgzx1yqras4ggmsc40";
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

  solc = callPackage ((import ./solc-versions.nix).solc_0_4_20) {};
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
      (versioned "symbex" (x: haskellPackages.callPackage x {}));

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
    version = "1.8.1";
    src = self.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "go-ethereum";
      rev = "v${version}";
      sha256 = "0k7ly9cw68ranksa1fdn7v2lncmlqgabw3qiiyqya2xz3s4aazlf";
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

  sicc = let
    version = "1";
    AgdaStdlib = self.AgdaStdlib.overrideDerivation (_: {
      src = self.fetchFromGitHub {
        owner = "agda";
        repo = "agda-stdlib";
        rev = "301343525ab919a8c95d89fe15972010c272c2a8";
        sha256 = "0w18czndsqyasfvgwlsi63f1cnc72hdgj46d7ndybp3a0c4xp2na";
      };
    });
    sic = stdenv.mkDerivation {
      name = "sic-${version}";
      src = ./SIC.agda;
      builder = ./sic.sh;
    };
    sic-unmangle = self.pkgs.writeTextFile {
      name = "sic-unmangle-${version}";
      executable = true;
      destination = "/bin/sic-unmangle";
      text = ''
#!${self.pkgs.nodejs}/bin/node
pad = (n, x) => new Array(n - x.length + 1).join("0") + x
xpad = (n, x) => pad(n * 2, Number(x).toString(16))
meme = x => x.replace(/BSig \\"(.*?)\\"/gm, (_, x) =>
   x.split("").map(y => `B1 ''${y.charCodeAt(0).toString()}`).join(" "))
scan = x => x.replace(/[^B]*B[124]\D+(\d+)[^B]*/gm, (_, n, x) => xpad(n, x))
cat = x => require("fs").readFileSync(x, { encoding: "utf-8" })
process.stdout.write(cat("/dev/stdin"))
      '';
    };
  in bashScript {
    inherit version;
    name = "sicc";
    deps = with self.pkgs; [coreutils haskellPackages.Agda gnugrep sic-unmangle];
    text = ''
      tmp=$(mktemp -d)
      cd "$tmp"
      cp ${sic}/share/agda/SIC.agda "$tmp"
      { echo IOTCM "\"$tmp/SIC.agda\"" NonInteractive Indirect \
          \( Cmd_load "\"$tmp/SIC.agda\"" \
          [\"--include-path=${AgdaStdlib}/share/agda\", \
          "\"--compile-dir=$tmp\""] \)
        echo IOTCM "\"$tmp/SIC.agda\"" None Indirect \
          \( Cmd_compute_toplevel DefaultCompute \"main\" \)
      } | agda --interaction | grep 'Normal Form' | tail -c +36 | head -c -10 | sic-unmangle
    '';
  };

  dapphub-emacs-experiment = let
    version = "1";
    dapphub-elisp = self.pkgs.writeTextFile {
      name = "dapphub-emacs-${version}";
      destination = "/dapphub.el";
      text = ''
        (package-initialize)
        (load-theme 'zenburn t)
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
  in bashScript {
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
