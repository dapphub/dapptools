{ stdenv, fetchFromGitHub, cmake, boost, gmp, procps, openssl, pkgconfig }:

stdenv.mkDerivation rec {
  name = "libff-${version}";
  version = "0.20181030";
  src = fetchFromGitHub {
    owner = "scipr-lab";
    repo = "libff";
    rev = "f2067162520f91438b44e71a2cab2362f1c3cab4";
    sha256 = "0bkkw7g6jmz2xx6rci8pj0w7bf6m0ss9clazxgd4mcizb8pb9siv";
    fetchSubmodules = true;
  };

  patches = [./libff.patch];

  nativeBuildInputs = [cmake pkgconfig];
  buildInputs = [boost gmp procps openssl];
  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "C++ library for Finite Fields and Elliptic Curves";
    homepage = https://github.com/scipr-lab/libff;
    license = licenses.mit;
  };
}
