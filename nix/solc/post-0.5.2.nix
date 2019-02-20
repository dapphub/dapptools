version: rev: sha256:
{ stdenv, fetchzip, fetchFromGitHub, boost, cmake,
  z3Support ? true, z3 ? null
}:

assert z3Support -> z3 != null;

let
  jsoncppURL = https://github.com/open-source-parsers/jsoncpp/archive/1.8.4.tar.gz;
  jsoncpp = fetchzip {
    url = jsoncppURL;
    sha256 = "1z0gj7a6jypkijmpknis04qybs1hkd04d1arr3gy89lnxmp6qzlm";
  };
in
stdenv.mkDerivation {
  name = "solc-${version}";

  src = fetchFromGitHub {
    owner = "ethereum";
    repo = "solidity";
    inherit rev sha256;
  };

  patches = [
    ./patches/post-0.5.2.patch
  ];

  postPatch = ''
    touch prerelease.txt
    echo >commit_hash.txt "${rev}"
    substituteInPlace cmake/jsoncpp.cmake \
      --replace "${jsoncppURL}" ${jsoncpp}
  '';

  cmakeFlags = [
    "-DBoost_USE_STATIC_LIBS=OFF"
    "-DBUILD_SHARED_LIBS=ON"
  ] ++ stdenv.lib.optionals (!z3Support) [
    "-DUSE_Z3=OFF"
  ];


  doCheck = stdenv.hostPlatform.isLinux && stdenv.hostPlatform == stdenv.buildPlatform;
  checkPhase = "LD_LIBRARY_PATH=./libsolc:./libsolidity:./liblll:./libevmasm:./libdevcore:./libyul:./liblangutil:$LD_LIBRARY_PATH " +
               "./test/soltest -p -- --no-ipc --no-smt --testpath ../test";

  nativeBuildInputs = [ cmake ];
  buildInputs = [ boost ] ++ stdenv.lib.optionals z3Support [ z3 ];

  outputs = [ "out" "dev" ];

  meta = with stdenv.lib; {
    description = "Compiler for Ethereum smart contract language Solidity";
    homepage = https://github.com/ethereum/solidity;
    license = licenses.gpl3;
    platforms = with platforms; linux ++ darwin;
    maintainers = with maintainers; [ dbrock akru ];
    inherit version;
  };
}
