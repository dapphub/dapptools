{ ... }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

  ethereum-test-suite = x: x.fetchFromGitHub {
    owner = "ethereum";
    repo = "tests";
    rev = "da6d391922cb0e3c6bda24871c89d33bc815c1dc";
    sha256 = "06h3hcsm09kp4hzq5sm9vqkmvx2nvgbh5i788qnqh5iiz9fpaa9k";
  };

  hevmCompliance = x: x.runCommand "hevm-compliance" {} ''
    mkdir -p $out/nix-support
    export PATH=${x.pkgs.hevm}/bin:$PATH
    ${x.pkgs.hevm}/bin/hevm compliance \
      --tests ${ethereum-test-suite x} \
      --skip modexp \
      --html > $out/index.html
    echo report testlog $out index.html > $out/nix-support/hydra-build-products
  '';

  # These packages should always work and be available in the binary cache.
  stable = dist: with dist.pkgs; {
    inherit dai;
    inherit dapp-which;
    inherit dapp;
    inherit ethsign;
    inherit evmdis;
    inherit go-ethereum-unlimited;
    inherit go-ethereum;
    inherit hevm;
    inherit qrtx-term;
    inherit qrtx;
    inherit seth;
    inherit setzer;
    inherit token;

    hevm-compliance = hevmCompliance dist;
  # the union is necessary because nix-build does not evaluate sets
  # recursively, and `solc-versions` is a set
  } // dist.pkgs.solc-versions ;

in {
  dapphub.linux.stable = stable linux;
  dapphub.darwin.stable = stable darwin;
}
