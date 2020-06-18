let dapptools = import ../../. {};
in dapptools.mkShell {
  buildInputs = with dapptools; [
  dapp
  seth
  git
  ];
}
