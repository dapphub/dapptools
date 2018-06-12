{ stdenv, secp256k1 }:

stdenv.mkDerivation rec {
  name = "libethjet-${version}";
  version = "0.5";
  src = ./.;
  meta = with stdenv.lib; {
    description = "C library for Ethereum precompiled contracts";
    homepage = https://github.com/dapphub/libethjet;
    license = [licenses.mit];
    platforms = platforms.unix;
  };

  buildInputs = [secp256k1];
  installFlags = ["PREFIX=$(out)"];
}
