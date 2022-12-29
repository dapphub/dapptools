{
  pkgs ? import ../.. {}
}:

with pkgs;

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
    seth
    solc
    util-linux
    which
  ];
  LANG="en_US.UTF-8";
}
