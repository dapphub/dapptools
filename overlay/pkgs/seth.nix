{ stdenv, makeWrapper, lib, fetchFromGitHub
, bc, coreutils, curl, ethabi, git, gnused, jshon, perl, solc, which, nodejs }:

stdenv.mkDerivation rec {
  name = "seth-${version}";
  version = "0.5.7";

  src = fetchFromGitHub {
    owner = "dapphub";
    repo = "seth";
    rev = "v${version}";
    sha256 = "08xzfp713w2z5qfiikv935phs0yi4f3a5rqnfghh5y5kcvpwsqr4";
  };

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    bc coreutils curl ethabi git gnused jshon perl solc which nodejs
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
