{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

  ethereum-test-suite = x: x.fetchFromGitHub {
    owner = "ethereum";
    repo = "tests";
    rev = "644967e345bbc6642fab613e1b1737abbe131f78";
    sha256 = "1c76xri5qhbqmd088k3s1wldkys5qrsqyx2a2m1903an41w5bz5f";
  };

  # run all General State Tests, skipping tests that deal with "anomalies on the main network"
  # (see section K.1 of https://ethereum.github.io/yellowpaper/paper.pdf), and some performance
  # heavy ones.
  hevmCompliance = x: x.runCommand "hevm-compliance" {} ''
    mkdir "$out"
    export PATH=${x.pkgs.hevm}/bin:${x.pkgs.jq}/bin:$PATH
    ${x.pkgs.hevm}/bin/hevm compliance \
      --tests ${ethereum-test-suite x} \
      --skip "(Create2Recursive|Create1000|recursiveCreateReturn|underflowTest|static_Call50000|Return5000)" \
      --timeout 20 \
      --html > $out/index.html
  # Disable obsolete VMTests - gas expectations broken by Istanbul
  #  ${x.pkgs.hevm}/bin/hevm compliance \
  #    --tests ${ethereum-test-suite x} \
  #    --group "VM"
  '';

  # These packages should always work and be available in the binary cache.
  stable = dist: with dist.pkgs; {
    inherit dapp;
    inherit ethsign;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit hevm;
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
