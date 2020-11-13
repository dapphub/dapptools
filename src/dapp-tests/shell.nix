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
  buildInputs = [ killall cacert bashInteractive curl dapp gnumake hevm procps seth solc go-ethereum python-with-pkgs ];
}
