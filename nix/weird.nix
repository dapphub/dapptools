# Some weird experiments that aren't really used.

self: super:

let
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

  callPackage = self.pkgs.callPackage;
  lib = self.pkgs.lib;
  stdenv = self.pkgs.stdenv;

in {
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

  ethos-iso = makeIso {
    module = import ./ethos.nix { hidpi = false; };
    config.isoImage.appendToMenuLabel = " (Ethos by DappHub)";
  };

  ethos-iso-hidpi = makeIso {
    module = import ./ethos.nix { hidpi = true; };
    config.isoImage.appendToMenuLabel = " (Ethos by DappHub, HiDPI)";
  };

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
}