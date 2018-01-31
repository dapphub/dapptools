{ stdenv, makeWrapper, lib, fetchFromGitHub
, seth, curl, jshon, bc, gnused, which, perl
, datamash
}:

stdenv.mkDerivation rec {
  name = "setzer-${version}";
  version = "0.1.17";
  src = ./.;

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    seth curl jshon bc gnused which perl datamash
  ]; in ''
    wrapProgram "$out/bin/setzer" --prefix PATH : "${path}"
  '';

  meta = with lib; {
    description = "Ethereum price feed tool";
    homepage = https://github.com/makerdao/setzer;
    license = licenses.gpl3;
    inherit version;
  };
}
