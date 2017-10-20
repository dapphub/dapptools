{ lib, stdenv, fetchFromGitHub, makeWrapper
, seth, git, solc, shellcheck, nodejs, hevm, jshon, nix }:

stdenv.mkDerivation rec {
  name = "dapp-${version}";
  version = "0.6.4";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "dapp";
    rev = "v${version}";
    sha256 = "0cqqng4sp1rdcrhnb0r3vqhicjhj8hrwgd4hcm97glkg54z0ys1y";
  };

  nativeBuildInputs = [makeWrapper shellcheck];
  buildPhase = "true";
  doCheck = true;
  checkPhase = "make test";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    nodejs solc git seth hevm jshon nix
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
