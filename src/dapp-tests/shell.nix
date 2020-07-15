{
  pkgs ? import ../.. {}
}:

pkgs.mkShell {
  name = "dapp-tests";

  buildInputs = with pkgs; [ bash curl dapp seth solc ];

  shellHook = ''
    ./integration/tests.sh
  '';
}
