{ lib, stdenv, fetchFromGitHub, makeWrapper, glibcLocales, solc, nix
, bc, coreutils, curl, ethsign, git, gnused, jq, jshon, nodejs, tre, perl
, gnugrep, hevm, shellcheck, dapptoolsSrc, eth-utils }:

stdenv.mkDerivation rec {
  name = "seth-${version}";
  version = "0.12.0";
  src = ./.;

  nativeBuildInputs = [ nodejs makeWrapper shellcheck ];
  dontBuild = true;
  doCheck = true;
  checkTarget = "test";
  makeFlags = ["prefix=$(out)"];

  postInstall =
    let
      path = lib.makeBinPath [
        bc coreutils curl ethsign git gnused nix jq hevm jshon nodejs tre perl solc
        gnugrep eth-utils
      ];
    in
      ''
        wrapProgram "$out/bin/seth" \
          --prefix PATH : ${path} \
          --set DAPPTOOLS ${dapptoolsSrc} \
        ${lib.optionalString (glibcLocales != null) ''
          --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
      ''}
  '';

  # the patching of nodejs shebangs is needed by the seth invocations in
  # src/dapp-tests/integration/tests.sh.
  # that's also the reason why nodejs is added to nativeBuildInputs
  postFixup = ''
    patchShebangs $out/libexec/seth
  '';

  meta = {
    description = "Command-line client for talking to Ethereum nodes";
    homepage = https://github.com/dapphub/dapptools/src/seth/;
    maintainers = [lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
