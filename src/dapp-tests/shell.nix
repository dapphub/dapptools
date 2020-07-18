{
  pkgs ? import ../.. {}
}:

pkgs.mkShell {
  name = "dapp-tests";

  buildInputs = with pkgs; [ killall cacert bashInteractive curl dapp gnumake hevm procps seth solc ];
}
