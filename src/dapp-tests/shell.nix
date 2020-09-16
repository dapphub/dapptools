{
  pkgs ? import ../.. {}
}:

pkgs.mkShell {
  name = "dapp-tests";

  buildInputs = with pkgs; [ coreutils killall cacert bashInteractive curl dapp gnumake hevm procps seth solc ];
}
