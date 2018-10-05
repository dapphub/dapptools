{ stdenv, lib, buildGo19Package, fetchFromGitHub }:

buildGo19Package rec {
  name = "evmdis-${version}";
  version = "0.20170616";
  goPackagePath = "github.com/Arachnid/evmdis";

  src = fetchFromGitHub {
    owner = "Arachnid";
    repo = "evmdis";
    rev = "1abeda0a402b0aa4f755d571565d7044b3e56c77";
    sha256 = "0mjh9rcpdwd96m68pps1hhrsn9004g052hq8y2gc579pq50izl0a";
  };

  meta = with stdenv.lib; {
    homepage = https://github.com/Arachnid/evmdis;
    description = "Ethereum EVM disassembler";
    license = [ licenses.asl20 ];
    maintainers = [ maintainers.dbrock ];
  };
}
