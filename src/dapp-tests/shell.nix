{
  pkgs ? import ../.. {}
}:

pkgs.mkShell {
  name = "dapp-tests";

  buildInputs = with pkgs; [ bash curl dapp gnumake seth solc ];
}
