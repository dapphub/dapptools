{ lib, stdenv, fetchFromGitHub, makeWrapper, glibcLocales
, seth, git, solc, shellcheck, nodejs, hevm, jshon, nix, coreutils }:

stdenv.mkDerivation rec {
  name = "dapp-${version}";
  version = "0.8.3";
  src = ./.;

  nativeBuildInputs = [makeWrapper shellcheck coreutils];
  buildPhase = "true";
  doCheck = true;
  checkPhase = "make test";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    nodejs solc git seth hevm jshon nix coreutils
  ]; in ''
    wrapProgram "$out/bin/dapp" --prefix PATH : "${path}" \
      ${if glibcLocales != null then
        "--set LOCALE_ARCHIVE \"${glibcLocales}\"/lib/locale/locale-archive"
        else ""}
  '';

  meta = {
    description = "Simple tool for creating Ethereum-based dapps";
    homepage = https://github.com/dapphub/dapp/;
    maintainers = [stdenv.lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
