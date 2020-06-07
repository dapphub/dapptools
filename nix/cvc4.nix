{ stdenv, fetchFromGitHub, cmake, cln, gmp, git, swig, pkgconfig
, readline, libantlr3c, boost, jdk, autoreconfHook
, python3, antlr3_4
}:

stdenv.mkDerivation rec {
  pname = "cvc4";
  version = "1.8-prerelease";

  src = fetchFromGitHub {
    owner  = "cvc4";
    repo   = "cvc4";
    rev    = "fd60da4a22f02f6f5b82cef3585240c1b33595e9";
    sha256 = "1bh0dvqskny3m95awlk237cdv0wds3qw1n89i3am1fqq0gs4ivyj";
  };

  nativeBuildInputs = [ pkgconfig cmake ];
  buildInputs = [ gmp git python3.pkgs.toml cln readline swig libantlr3c antlr3_4 boost jdk python3 ];
  configureFlags = [
    "--enable-language-bindings=c,c++,java"
    "--enable-gpl"
    "--with-cln"
    "--with-readline"
    "--with-boost=${boost.dev}"
  ];

  prePatch = ''
    patch -p1 -i ${./minisat-fenv.patch} -d src/prop/minisat
    patch -p1 -i ${./minisat-fenv.patch} -d src/prop/bvminisat
  '';

  preConfigure = ''
    patchShebangs ./src/
  '';
  cmakeFlags = ''
  -DCMAKE_BUILD_TYPE=Production
  '';

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "A high-performance theorem prover and SMT solver";
    homepage    = http://cvc4.cs.stanford.edu/web/;
    license     = licenses.gpl3;
    platforms   = platforms.unix;
    maintainers = with maintainers; [ vbgl thoughtpolice gebner ];
  };
}
