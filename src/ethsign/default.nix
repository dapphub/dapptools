{ stdenv, buildGoModule }:

buildGoModule rec {
  name = "ethsign-${version}";
  version = "0.16.2";

  src = ./.;

  vendorSha256 = "1p7gkpv88v6swz8dpjvrzfaa2jkpr5xw26bd3rjazv5wcs6ipwy7";
  runVend = true;

  meta = with stdenv.lib; {
    homepage = http://github.com/dapphub/dapptools;
    description = "Make raw signed Ethereum transactions";
    license = [licenses.agpl3];
  };
}
