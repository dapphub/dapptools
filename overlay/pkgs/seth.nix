{ stdenv, makeWrapper, lib, fetchFromGitHub
, bc, coreutils, curl, ethabi, git, gnused, jshon, perl, solc, which }:

stdenv.mkDerivation rec {
  name = "seth-${version}";
  version = "0.5.6.20171018";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "seth";
    rev = "72d877dfd207b93bda03c85bfa67238892252f70";
    sha256 = "09m9hmz346sd81430363sbih8vhh4c6jgmn024lj0msric0ny106";
  };

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    bc coreutils curl ethabi git gnused jshon perl solc which
  ]; in ''
    wrapProgram "$out/bin/seth" --prefix PATH : "${path}"
  '';

  meta = {
    description = "Command-line client for talking to Ethereum nodes";
    homepage = https://github.com/dapphub/seth/;
    maintainers = [stdenv.lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
