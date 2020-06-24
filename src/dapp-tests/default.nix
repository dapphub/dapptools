{ pkgs }:

let
  ds-test-src = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "ds-test";
    rev = "eb7148d43c1ca6f9890361e2e2378364af2430ba";
    sha256 = "1phnqjkbcqg18mh62c8jq0v8fcwxs8yc4sa6dca4y8pq2k35938k";
  } + "/src";

  ds-test = pkgs.buildDappPackage {
    src = ds-test-src;
    name = "ds-test";
    doCheck = false;
  };

in
  pkgs.buildDappPackage {
    name = "dapp-tests";
    src = ./.;
    deps = [ ds-test ];

    preInstallPhases = [ "postCheckPhase" ];
    checkInputs = with pkgs; [ hevm jq seth ];
    postCheckPhase = ''
      patchShebangs integration
      make
    '';
  }
