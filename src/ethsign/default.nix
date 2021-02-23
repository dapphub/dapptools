{ stdenv, buildGoModule }:

buildGoModule rec {
  name = "ethsign-${version}";
  version = "0.16.0";

  src = ./.;

  vendorSha256 = "193yjzyf6khzanvqalbdqccwzm11nq9z3aykni2q8fj7r5j8l4v0";
  runVend = true;

  meta = with stdenv.lib; {
    homepage = http://github.com/dapphub/dapptools;
    description = "Make raw signed Ethereum transactions";
    license = [licenses.agpl3];
  };
}
