{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

in rec {
  dapphub-linux = with linux.pkgs; {
    inherit dapp;
    inherit hsevm;
    inherit keeper;
    inherit setzer;
    inherit solc-versions;
    inherit go-ethereum;
  };

  dapphub-darwin = with darwin.pkgs; {
    inherit dapp;
    inherit hsevm;
    # inherit keeper;
    inherit setzer;
    inherit solc-versions;
    inherit go-ethereum;
  };

  hsevm-consensus-test = rec {
    ethereum-test-suite = linux.pkgs.fetchFromGitHub {
      owner = "ethereum";
      repo = "tests";
      rev = "7e361956bd68f5cac72fe41f29e5734ee94ae2de";
      sha256 = "0l5qalgbscr77vjhyf7b542055wnp4pddpfslnypp5sqws5w940w";
    };

    hsevm-test-report = linux.pkgs.runCommand "hsevm-test-report" {} ''
      mkdir -p $out/nix-support
      export PATH=${dapphub-linux.hsevm}/bin:$PATH
      ${dapphub-linux.hsevm}/bin/hsevm vm-test-report \
        --tests ${ethereum-test-suite} > $out/index.html
      echo report testlog $out index.html > $out/nix-support/hydra-build-products
    '';
  };
}
