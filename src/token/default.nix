{ lib, stdenv, makeWrapper, coreutils, perl, seth, glibcLocales }:

stdenv.mkDerivation rec {
  name = "token-${version}";
  version = "0.5.1";
  src = ./.;

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];

  postInstall =
    let
      path = lib.makeBinPath [ coreutils perl seth ];
    in
      ''
        wrapProgram "$out/bin/token" \
          --prefix PATH : ${path} \
        ${lib.optionalString (glibcLocales != null) ''
          --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive
      ''}
  '';

  meta = {
    description = "Command-line tool for ERC20 tokens";
    homepage = https://github.com/dapphub/src/token;
    license = lib.licenses.gpl3;
    inherit version;
  };
}
