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

  # These packages should always work and be available in the binary cache.
  stable = dist: with dist.pkgs; {
    inherit dai;
    inherit dapp-which;
    inherit dapp;
    inherit ethjet;
    inherit ethsign;
    inherit evmdis;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit hevm;
    inherit keeper;
    inherit qrtx-term;
    inherit qrtx;
    inherit seth;
    inherit setzer;
    inherit solc-versions;
    inherit token;

    hevm-test-report = hevmTestReport dist;
  };

  # These packages are semi-unmaintained or experimental.
  weird = dist: with dist.pkgs; {
    inherit celf;
    inherit ds-chief;
    inherit hevmas;
    inherit hevml;
    inherit oasis-orders;
    inherit tla-plus;
    inherit mkbip39;
    inherit myetherwallet;
    inherit symbex;
    inherit dafny;
  };

in {
  dapphub.linux.stable = stable linux;
  dapphub.linux.weird = weird linux;
  dapphub.darwin.stable = stable darwin;
  dapphub.darwin.weird = weird darwin;
}
