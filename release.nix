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

in rec {

  dapphub.linux.stable = with linux.pkgs; {
    inherit celf;
    inherit dai;
    inherit dapp-which;
    inherit dapp;
    inherit ds-chief;
    inherit ethjet;
    inherit ethsign;
    inherit evmdis;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit hevm;
    inherit hevmas;
    inherit hevml;
    inherit keeper;
    inherit mkbip39;
    inherit myetherwallet;
    inherit oasis-orders;
    inherit qrtx-term;
    inherit qrtx;
    inherit seth;
    inherit setzer;
    inherit solc-versions;
    inherit tla-plus;
    inherit token;
    inherit symbex;

    hevm-test-report = hevmTestReport linux;
  };

  dapphub.linux.master = with linux.master.pkgs; {
    inherit dapp;
    inherit hevm;
    inherit hevmas;
    inherit seth;

    hevm-test-report = hevmTestReport linux.master;
  };

  dapphub.darwin.stable = with darwin.pkgs; {
    inherit celf;
    inherit dai;
    inherit dapp-which;
    inherit dapp;
    inherit ds-chief;
    inherit ethjet;
    inherit ethsign;
    inherit evmdis;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit hevm;
    inherit hevml;
    inherit mkbip39;
    inherit myetherwallet;
    inherit oasis-orders;
    inherit qrtx-term;
    inherit seth;
    inherit setzer;
    inherit solc-versions;
    inherit token;
  };

  dapphub.darwin.master = with darwin.master.pkgs; {
    inherit dapp;
    inherit hevm;
    inherit seth;
  };
}
