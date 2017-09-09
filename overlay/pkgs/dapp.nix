{ lib, stdenv, fetchFromGitHub, makeWrapper
, seth, git, solc, shellcheck, nodejs, hsevm }:

stdenv.mkDerivation rec {
  name = "dapp";
  version = "0.5.6";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "dapp";
    rev = "v${version}";
    sha256 = "03fiyqdlvg90h389qmhd0494b767krymasmz5mrzmcqpj17hmx5p";
  };

  nativeBuildInputs = [makeWrapper shellcheck];
  buildPhase = "true";
  doCheck = true;
  checkPhase = "make test";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    nodejs solc git seth hsevm
  ]; in ''
    wrapProgram "$out/bin/dapp" --prefix PATH : "${path}"
  '';

  meta = {
    description = "Simple tool for creating Ethereum-based dapps";
    homepage = https://github.com/dapphub/dapp/;
    maintainers = [stdenv.lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
