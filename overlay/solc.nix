version: rev: sha256:
{ stdenv, fetchzip, fetchgit, boost, cmake, z3 }:

let
  jsoncppURL = https://github.com/open-source-parsers/jsoncpp/archive/1.7.7.tar.gz;
  jsoncpp = fetchzip {
    url = jsoncppURL;
    sha256 = "0jz93zv17ir7lbxb3dv8ph2n916rajs8i96immwx9vb45pqid3n0";
  };
in

stdenv.mkDerivation {
  name = "solc-${version}";

  # Cannot use `fetchFromGitHub' because of submodules
  src = fetchgit {
    url = "https://github.com/ethereum/solidity";
    inherit rev sha256;
  };

  patchPhase = ''
    echo >commit_hash.txt '${rev}'
    echo >prerelease.txt
    substituteInPlace deps/jsoncpp.cmake \
      --replace '${jsoncppURL}' ${jsoncpp}
    substituteInPlace cmake/EthCompilerSettings.cmake \
      --replace 'add_compile_options(-Werror)' ""
  '' + stdenv.lib.optionalString stdenv.isDarwin ''
    substituteInPlace cmake/EthDependencies.cmake \
      --replace 'Boost_USE_STATIC_LIBS ON' 'Boost_USE_STATIC_LIBS OFF'
  '';

  # The Darwin flag for patch phase is a hack to avoid some
  # recompilation.  Actually the cmakeFlags way works fine, except not
  # in older versions.  I want to build those older version on Mac,
  # but not rebuild my Linux versions, so I do it this silly way.

  cmakeFlags = [
    "-DBoost_USE_STATIC_LIBS=OFF"
  ];

  buildInputs = [ boost cmake z3 ];

  meta = {
    description = "Compiler for Ethereum smart contract language Solidity";
    longDescription = "This package also includes `lllc', the LLL compiler.";
    homepage = https://github.com/ethereum/solidity;
    license = stdenv.lib.licenses.gpl3;
    platforms = with stdenv.lib.platforms; linux ++ darwin;
    maintainers = [ stdenv.lib.maintainers.dbrock ];
    inherit version;
  };
}
