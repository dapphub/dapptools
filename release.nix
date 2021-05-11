{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

  ethereum-test-suite = x: x.fetchFromGitHub {
    owner = "ethereum";
    repo = "tests";
    rev = "644967e345bbc6642fab613e1b1737abbe131f78";
    sha256 = "1525yzmc5wig5pkz904k2rqz11ygiql8mzb6djym9w87bymfvzm9";
  };

  # run all General State Tests, skipping performance heavy tests and the ones missing
  # postState.
  hevmCompliance = x: x.runCommand "hevm-compliance" {} ''
    mkdir "$out"
    export PATH=${x.pkgs.hevm}/bin:${x.pkgs.jq}/bin:$PATH
    ${x.pkgs.hevm}/bin/hevm compliance \
      --tests ${ethereum-test-suite x} \
      --skip "(Create2Recursive|Create1000|recursiveCreateReturn|underflowTest|walletRemoveOwnerRemovePending|Return5000|randomStatetest177|loopExp|loopMul)" \
      --timeout 20 \
      --html > $out/index.html
  # Disable obsolete VMTests - gas expectations broken by Istanbul
  #  ${x.pkgs.hevm}/bin/hevm compliance \
  #    --tests ${ethereum-test-suite x} \
  #    --group "VM"
  '';

  # These packages should always work and be available in the binary cache.
  stable = dist: with dist.pkgs; {
    inherit hevm;
    inherit hevmUnwrapped
    inherit dapp;
    inherit ethsign;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit qrtx-term;
    inherit qrtx;
    inherit seth;
    inherit token;
    inherit solc-versions;

    inherit dapp-tests;
    inherit hevm-tests;
    hevm-compliance = hevmCompliance dist;
  };

in {
  dapphub.linux.stable = stable linux;
  dapphub.darwin.stable = stable darwin;
}
