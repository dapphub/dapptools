{ lib, buildGoModule }:

buildGoModule rec {
  name = "ethsign-${version}";
  version = "0.17.1";

  src = ./.;

  vendorSha256 = "1zbsq1lyqinyzv5x4p1xgkxsyb7y92fbmf44gyaix34xrag5s27m";
  runVend = true;

  meta = {
    homepage = http://github.com/dapphub/dapptools;
    description = "Make raw signed Ethereum transactions";
    license = [lib.licenses.agpl3];
  };
}
