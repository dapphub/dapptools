{ lib, stdenv, fetchFromGitHub, makeWrapper, glibcLocales
, coreutils, git, gnused, gnumake, hevm, jshon, jq, nix
, nodejs, perl, python3, seth, shellcheck, solc, tre, dapptoolsSrc }:

stdenv.mkDerivation rec {
  name = "dapp-${version}";
  version = "0.35.0";
  src = ./.;

  nativeBuildInputs = [makeWrapper shellcheck coreutils nodejs python3];
  buildPhase = "true";
  doCheck = true;
  checkPhase = "make test";
  makeFlags = ["prefix=$(out)"];

  postInstall =
    let
      path = lib.makeBinPath [
        coreutils git gnused gnumake hevm jshon jq nix nodejs perl seth solc tre python3
      ];
    in
      ''
        wrapProgram "$out/bin/dapp" \
          --prefix PATH : ${path} \
          --set DAPPTOOLS ${dapptoolsSrc} \
        ${lib.optionalString (glibcLocales != null) ''
          --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
      ''}
  '';

  # the patching of python shebangs is needed by the python invocations in
  # src/dapp-tests/integration/tests.sh.
  # that's also the reason why nodejs is added to nativeBuildInputs
  postFixup = ''
    patchShebangs $out/libexec/dapp
  '';


  meta = {
    description = "Simple tool for creating Ethereum-based dapps";
    homepage = https://github.com/dapphub/dapptools/src/dapp/;
    maintainers = [lib.maintainers.dbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
