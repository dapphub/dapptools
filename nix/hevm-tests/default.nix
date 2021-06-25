{ pkgs }:
let
  solc = "${pkgs.solc-static-versions.solc_0_8_6}/bin/solc-0.8.6";
  solidity = pkgs.fetchFromGitHub {
    owner = "ethereum";
    repo = "solidity";
    rev = "b8d736ae0c506b1b3cf5d2456af67e8dc2c0ca8e"; # v0.6.7
    sha256 = "1zqfcfgy70hmckxb3l59rabdpzj7gf1vzg6kkw4xz0c6lzy7mrpz";
  };
  runWithSolver = file : solver : (import file) { inherit pkgs solc solidity solver; };
in
  pkgs.recurseIntoAttrs {
    yulEquivalence-z3 = runWithSolver ./yul-equivalence.nix "z3";
    yulEquivalence-cvc4 = runWithSolver ./yul-equivalence.nix "cvc4";

    # z3 takes 3hrs to run these tests on a fast machine, and even then ~180 timeout
    #smtChecker-z3 = runWithSolver ./smt-checker.nix "z3";
    smtChecker-cvc4 = runWithSolver ./smt-checker.nix "cvc4";
  }
