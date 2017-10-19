{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

in rec {
  dapphub-linux = with linux.pkgs; {
    inherit dapp;
    inherit seth;
    inherit seth-jays;
    inherit hevm;
    inherit hevm-profiling;
    inherit keeper;
    inherit setzer;
    inherit solc-versions;
    inherit go-ethereum;
  };

  dapphub-darwin = with darwin.pkgs; {
    inherit dapp;
    inherit seth;
    inherit seth-jays;
    inherit hevm;
    # inherit hevm-profiling;
    # inherit keeper;
    inherit setzer;
    inherit solc-versions;
    inherit go-ethereum;
  };

  hevm-development = rec {
    ethereum-test-suite = linux.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "tests";
      rev = "7e361956bd68f5cac72fe41f29e5734ee94ae2de";
      sha256 = "0l5qalgbscr77vjhyf7b542055wnp4pddpfslnypp5sqws5w940w";
    };

    hevm-test-report = linux.pkgs.runCommand "hevm-test-report" {} ''
      mkdir -p $out/nix-support
      export PATH=${dapphub-linux.hevm}/bin:$PATH
      ${dapphub-linux.hevm}/bin/hevm vm-test-report \
        --tests ${ethereum-test-suite} > $out/index.html
      echo report testlog $out index.html > $out/nix-support/hydra-build-products
    '';
  };
}
