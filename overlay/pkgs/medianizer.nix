{ stdenv, makeWrapper, lib, fetchFromGitHub
, seth, curl, jshon
}:

stdenv.mkDerivation rec {
  name = "medianizer-${version}";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "makerdao";
    repo = "medianizer";
    rev = "cb81d12d198d52b6e73214927b57261b472798ec";
    sha256 = "1xzv7j4xhsk9w5m0cwhfjmvpjln0fn28gwq3jmp4b7qib45fxcjz";
  };

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    seth curl jshon
  ]; in ''
    wrapProgram "$out/bin/medianizer" --prefix PATH : "${path}"
  '';

  meta = {
    description = "Ethereum median tool";
    homepage = https://github.com/makerdao/medianizer;
    maintainers = [stdenv.lib.maintainers.mbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
