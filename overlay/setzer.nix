{ stdenv, makeWrapper, lib, fetchFromGitHub
, seth, curl, jshon, bc, gnused, which, perl
}:

stdenv.mkDerivation rec {
  name = "setzer-${version}";
  version = "unstable-20171019";

  src = fetchFromGitHub {
    owner = "makerdao";
    repo = "setzer";
    rev = "6a0588f6d4fab502ebc09000cc03711eb5390458";
    sha256 = "1s6b2skalwsgimlpil9zir1dbciyzzh4ijp0i6x08w0h75afbpp5";
  };

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    seth curl jshon bc gnused which perl
  ]; in ''
    wrapProgram "$out/bin/setzer" --prefix PATH : "${path}"
  '';

  meta = with lib; {
    description = "Ethereum price feed tool";
    homepage = https://github.com/makerdao/setzer;
    maintainers = with maintainers; [dbrock];
    license = licenses.gpl3;
    inherit version;
  };
}
