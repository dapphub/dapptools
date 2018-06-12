{ stdenv, fetchFromGitHub, mlton }:

stdenv.mkDerivation rec {
  name = "celf-${version}";
  version = "unstable-2013-07-25";
  src = fetchFromGitHub {
    owner = "clf";
    repo = "celf";
    rev = "d61d95900ab316468ae850fa34a2fe9488bc5b59";
    sha256 = "0slrwcxglp0sdbp6wr65cdkl5wcap2i0fqxbwqfi1q3cpb6ph6hq";
  };
  buildInputs = [mlton];
  buildPhase = "make mlton";
  installPhase = ''
    mkdir -p $out/bin && cp celf $out/bin
  '';
}
