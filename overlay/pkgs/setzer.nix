{ stdenv, makeWrapper, lib, fetchFromGitHub
, seth, curl, jshon, bc, gnused, which, perl
}:

stdenv.mkDerivation rec {
  name = "setzer-${version}";
  version = "unstable-20170910";

  src = fetchFromGitHub {
    owner = "makerdao";
    repo = "setzer";
    rev = "1d27eed63b2cf636353f39eace3f56880413aa7d";
    sha256 = "0zfa4jz029g393ra6xr34insfb7gv8z8x8ang4rfxvvmydl2b9nk";
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
