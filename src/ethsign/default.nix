{ stdenv, buildGoModule, fetchFromGitHub, go-ethereum, fetchgit }:

buildGoModule rec {
  name = "ethsign-${version}";
  version = "0.16.0";

  src = ./.;

  modSha256 = "099hf9518zqgsjv050qlxj3la9f66j8bbbafh8vgivrw5vd81m60";
  
  meta = with stdenv.lib; {
    homepage = http://github.com/dapphub/dapptools;
    description = "Make raw signed Ethereum transactions";
    license = [licenses.agpl3];
  };
}
