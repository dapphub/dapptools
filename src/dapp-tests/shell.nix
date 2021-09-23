{
  pkgs ? import ../.. {}
}:

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    hypothesis
    pytest
    # other python packages you want
  ];
  python-with-pkgs = python3.withPackages my-python-packages;
in

mkShell {
  name = "dapp-tests";
  buildInputs = [
    bashInteractive
    bash_unit
    bc
    cacert
    coreutils
    curl
    dapp
    gnumake
    go-ethereum
    hevm
    jq
    killall
    procps
    python-with-pkgs
    seth
    solc
    util-linux
    which
  ];
  LANG="en_US.UTF-8";
}
