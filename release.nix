{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

  ethereum-test-suite = x: x.fetchFromGitHub {
    owner = "ethereum";
    repo = "tests";
    rev = "7e361956bd68f5cac72fe41f29e5734ee94ae2de";
    sha256 = "0l5qalgbscr77vjhyf7b542055wnp4pddpfslnypp5sqws5w940w";
  };

  hevmTestReport = x: x.runCommand "hevm-test-report" {} ''
    mkdir -p $out/nix-support
    export PATH=${x.pkgs.hevm}/bin:$PATH
    ${x.pkgs.hevm}/bin/hevm vm-test-report \
      --tests ${ethereum-test-suite x} > $out/index.html
    echo report testlog $out index.html > $out/nix-support/hydra-build-products
  '';

  makeIso = { module, config }:
    linux.pkgs.lib.hydraJob ((import ./nixpkgs/nixos/lib/eval-config.nix {
      system = "x86_64-linux";
      modules = [module config];
      pkgs = linux.pkgs;
    }).config.system.build.isoImage);

in rec {
  dapphub.ethos = makeIso {
    module = import ./ethos.nix { hidpi = false; };
    config = {
      isoImage.appendToMenuLabel = " (Ethos by DappHub)";
    };
  };

  dapphub.ethos-hidpi = makeIso {
    module = import ./ethos.nix { hidpi = true; };
    config = {
      isoImage.appendToMenuLabel = " (Ethos by DappHub, HiDPI)";
    };
  };

  dapphub.linux.stable = with linux.pkgs; {
    inherit dapp;
    inherit seth;
    inherit hevm;
    inherit hevml;
    inherit evmdis;
    inherit keeper;
    inherit setzer;
    inherit solc-versions;
    inherit go-ethereum;
    inherit go-ethereum-unlimited;
    inherit hevmas;
    inherit ethsign;
    inherit qrtx;
    inherit qrtx-term;

    hevm-test-report = hevmTestReport linux;
  };

  dapphub.linux.master = with linux.master.pkgs; {
    inherit dapp;
    inherit seth;
    inherit hevm;
    inherit hevmas;

    hevm-test-report = hevmTestReport linux.master;
  };

  dapphub.darwin.stable = with darwin.pkgs; {
    inherit dapp;
    inherit seth;
    inherit hevm;
    inherit hevml;
    inherit evmdis;
    # inherit keeper;
    inherit setzer;
    inherit solc-versions;
    inherit go-ethereum;
    inherit go-ethereum-unlimited;
    # inherit hevmas;
    inherit ethsign;
    inherit qrtx-term;
  };

  dapphub.darwin.master = with darwin.master.pkgs; {
    inherit dapp;
    inherit seth;
    inherit hevm;
    inherit dappsys;
    inherit dappsys-legacy;
    # inherit hevmas;
  };
}
