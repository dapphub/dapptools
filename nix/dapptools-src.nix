{ stdenv }:

stdenv.mkDerivation {
  name = "dapptools-src";
  src = ../.;

  phases = [ "unpackPhase" "installPhase" ];

  installPhase = ''
    cp -R . $out
  '';
}
