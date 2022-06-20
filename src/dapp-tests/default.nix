{ pkgs }:

let
  solc-0_8_6 = "${pkgs.solc-static-versions.solc_0_8_6}/bin/solc-0.8.6";
  solc-0_7_6 = "${pkgs.solc-static-versions.solc_0_7_6}/bin/solc-0.7.6";
  solc-0_6_7 = "${pkgs.solc-static-versions.solc_0_6_7}/bin/solc-0.6.7";

  ds-test-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-test";
    rev = "c0b770c04474db28d43ab4b2fdb891bd21887e9e";
    sha256 = "0cdzva82kkvj7ck4yf3ffma2n5rfa7miknjnz6jwgfy416a865m0";
  } + "/src";

  ds-token-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-token";
    rev = "6eb8a5010aad2980645c34c49cbfc1e9aea12c32";
    sha256 = "1rk0vg8gql8rmd399m2v9y86m2g1q8mbcrv97bx0p1wf8k983vym";
  } + "/src";

  ds-math-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-math";
    rev = "fa45427c0e0bb487335ae99a2e39564b3983a355";
    sha256 = "1giimmhlm2gxd7v794nbhykj61pv8ib60jv80ykx5smxh9lf4irr";
  } + "/src";

  ds-auth-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-auth";
    rev = "8035510b0bdfe90de9e29cac2c538f5ce0d89aea";
    sha256 = "1j7c60vzcg6cwc3d72q2sh6lzfbm04ipzn7nf8mfcli2yyhap6n1";
  } + "/src";

  ds-value-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-value";
    rev = "2e3d06ba1e96dc531f14d0749fa92146b7390ed8";
    sha256 = "1by3q3ndfmpgijckjhbyw4vvbzykg66v8yrl5awvpb76i8z3l8hf";
  } + "/src";

  ds-thing-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-thing";
    rev = "baf65c78908c846f4e45bb585ee5714551c1492d";
    sha256 = "173hvha10nb9l3p66dv4i1h737knkrvm34y5j5cvmq4g7y8vaa6d";
  } + "/src";

  ds-note-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-note";
    rev = "4f2ad380e41e664802c4cb3a34f139ac08700bd8";
    sha256 = "0a9qc04s6hwz2fiwhl4psfga4wnwi6s33fvpcaq3y0gagrrmjidn";
  } + "/src";

  dss-src = pkgs.fetchFromGitHub {
    owner = "makerdao";
    repo = "dss";
    rev = "36dab5c9fdce4fc36812e4fd8228be3beb18774d";
    sha256 = "1h82zw9qkb6wxwfc0ffz2lbmmmh0pj6s76jb23sqqcmy1wfnld1h";
    fetchSubmodules = false;
  } + "/src";

  ds-test = pkgs.buildDappPackage {
    src = ds-test-src;
    name = "ds-test";
    doCheck = false;
  };

  ds-math = pkgs.buildDappPackage {
    src = ds-math-src;
    name = "ds-math";
    doCheck = false;
    deps = [ ds-test ];
  };

  ds-auth = pkgs.buildDappPackage {
    solc = solc-0_7_6;
    src = ds-auth-src;
    name = "ds-auth";
    doCheck = false;
    deps = [ ds-test ];
  };

  ds-token = pkgs.buildDappPackage {
    solc = solc-0_7_6;
    src = ds-token-src;
    name = "ds-token";
    doCheck = false;
    deps = [ ds-test ds-math ds-auth ];
  };

  ds-note = pkgs.buildDappPackage {
    solc = solc-0_6_7;
    src = ds-note-src;
    name = "ds-note";
    doCheck = false;
    deps = [ ds-test ];
  };

  ds-thing = pkgs.buildDappPackage {
    solc = solc-0_6_7;
    src = ds-thing-src;
    name = "ds-thing";
    doCheck = false;
    deps = [ ds-test ds-auth ds-math ds-note ];
  };

  ds-value = pkgs.buildDappPackage {
    solc = solc-0_6_7;
    src = ds-value-src;
    name = "ds-value";
    doCheck = false;
    deps = [ ds-test ds-thing ];
  };

  runTest = { dir, shouldFail, name, dappFlags?"" }: pkgs.buildDappPackage {
    inherit name shouldFail;
    solc=solc-0_6_7;
    src = dir;
    dappFlags = "${dappFlags}";
    deps = [ ds-test ds-token ds-math ];
    checkInputs = with pkgs; [ hevm jq seth dapp solc ];
  };
in
  pkgs.recurseIntoAttrs {
    shouldPass = runTest {
      dir = ./pass;
      name = "dappTestsShouldPass";
      shouldFail = false;
      dappFlags = "--max-iterations 50 --smttimeout 600000 --ffi";
    };

    libraries0_8 = pkgs.buildDappPackage {
      name = "libraries-0.8";
      shouldFail = false;
      solc=solc-0_8_6;
      src = pkgs.runCommand "src" {} ''
        mkdir -p $out
        cp ${./pass/libraries.sol} $out/libraries.sol;
      '';
      deps = [ ds-test ];
      checkInputs = with pkgs; [ hevm jq seth dapp solc ];
    };

    shouldFail = let
      fail = match : runTest {
        dir = ./fail;
        shouldFail = true;
        name = "dappTestsShouldFail-${match}";
        dappFlags = "--match ${match} --smttimeout 600000";
      };
    in pkgs.recurseIntoAttrs {
      prove-add = fail "prove_add";
      prove-fail-call = fail "proveFail_shouldFail";
      prove-multi = fail "prove_multi";
      prove-smtTimeout = fail "prove_smtTimeout";
      prove-mul = fail "prove_mul";
      prove-distributivity = fail "prove_distributivity";
      prove-transfer = fail "prove_transfer";
      try-ffi = fail "testBadFFI";
      invariant-first = fail "invariantFirst";
      invariant-test-usr-bal = fail "invariantTestUserBal";
      invariant-test-initial-inv-call = fail "invariantCount";
      withdraw = fail "proveFail_withdraw";
    };

    dss = pkgs.buildDappPackage {
      solc = solc-0_6_7;
      src = dss-src;
      name = "dss";
      dappFlags = "--match '[^dai].t.sol'";
      deps = [ ds-test ds-token ds-value ];
    };
  }
