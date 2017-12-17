{ lib, stdenv, makeWrapper, coreutils, perl, seth }:

stdenv.mkDerivation rec {
  name = "token-${version}";
  version = "0.5";
  src = ./.;

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    coreutils perl seth
  ]; in ''
    wrapProgram "$out/bin/token" --prefix PATH : "${path}"
  '';

  meta = {
    description = "Command-line tool for ERC20 tokens";
    homepage = https://github.com/dapphub/token;
    license = lib.licenses.gpl3;
    inherit version;
  };
}
