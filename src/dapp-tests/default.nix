{ pkgs }:

let
  ds-test-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-test";
    rev = "eb7148d43c1ca6f9890361e2e2378364af2430ba";
    sha256 = "1phnqjkbcqg18mh62c8jq0v8fcwxs8yc4sa6dca4y8pq2k35938k";
  } + "/src";

  ds-token-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-token";
    rev = "8b752565a433f7326d38499c0c01cc7d9058721e";
    sha256 = "1rk0vg8gql8rmd399m2v9y86m2g1q8mbcrv97bx0p1wf8k983vym";
  } + "/src";

  ds-math-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-math";
    rev = "d0ef6d6a5f1de54a16e5ebb7af4d62bb9632de3e";
    sha256 = "1giimmhlm2gxd7v794nbhykj61pv8ib60jv80ykx5smxh9lf4irr";
  } + "/src";

  ds-auth-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-auth";
    rev = "434bf463b255ecc2157b9c671139f6c617795ebf";
    sha256 = "0lf5kzb9gd2d132bnz4xn8z6fgrq4pmg9fknhj6hig0c3g41a7sy";
  } + "/src";

  ds-value-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-value";
    rev = "f3dc6c8656727b74a5469ec2d4cd6851c9db8999";
    sha256 = "1by3q3ndfmpgijckjhbyw4vvbzykg66v8yrl5awvpb76i8z3l8hf";
  } + "/src";

  ds-thing-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-thing";
    rev = "f37879a41e817f54db43728cb39178cd99311289";
    sha256 = "173hvha10nb9l3p66dv4i1h737knkrvm34y5j5cvmq4g7y8vaa6d";
  } + "/src";

  ds-note-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-note";
    rev = "c673c9d1a1464e973db4489221e22dc5b9b02319";
    sha256 = "0a9qc04s6hwz2fiwhl4psfga4wnwi6s33fvpcaq3y0gagrrmjidn";
  } + "/src";

  dss-src = pkgs.fetchFromGitHub {
    owner = "makerdao";
    repo = "dss";
    rev = "36dab5c9fdce4fc36812e4fd8228be3beb18774d";
    sha256 = "1h82zw9qkb6wxwfc0ffz2lbmmmh0pj6s76jb23sqqcmy1wfnld1h";
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
    src = ds-auth-src;
    name = "ds-auth";
    doCheck = false;
    deps = [ ds-test ];
  };

  ds-token = pkgs.buildDappPackage {
    src = ds-token-src;
    name = "ds-token";
    doCheck = false;
    deps = [ ds-test ds-math ds-auth ];
  };

  ds-note = pkgs.buildDappPackage {
    src = ds-note-src;
    name = "ds-note";
    doCheck = false;
    deps = [ ds-test ];
  };

  ds-thing = pkgs.buildDappPackage {
    src = ds-thing-src;
    name = "ds-thing";
    doCheck = false;
    deps = [ ds-test ds-auth ds-math ds-note ];
  };

  ds-value = pkgs.buildDappPackage {
    src = ds-value-src;
    name = "ds-value";
    doCheck = false;
    deps = [ ds-test ds-thing ];
  };

  dss = pkgs.buildDappPackage {
    src = dss-src;
    name = "dss";
    doCheck = true;
    deps = [ ds-test ds-token ds-value ];
  };


  runTest = { dir, shouldFail, hevmFlags?"" }: pkgs.buildDappPackage {
    name = "dapp-tests";
    shouldFail = shouldFail;
    src = dir;
    hevmFlags = "${hevmFlags}";
    deps = [ ds-test ds-token ];
    checkInputs = with pkgs; [ hevm jq seth dapp solc ];
  };

in
  {
    runDssTests = dss;

    dappTestsShouldPass = runTest {
      dir = ./pass;
      shouldFail = false;
      hevmFlags = "--max-iterations 50";
    };

    dappTestsShouldFail = let
      fail = match : runTest { dir = ./fail; shouldFail = true; hevmFlags = "--match ${match}"; };
    in {
      prove-add = fail "prove_add";
      prove-fail-call = fail "proveFail_shouldFail";
      prove-multi = fail "prove_multi";
      prove-smtTimeout = fail "prove_smtTimeout";
      prove-transfer = fail "prove_transfer";
    };
  }
