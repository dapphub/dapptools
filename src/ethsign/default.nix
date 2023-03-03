{ lib, buildGoModule }:

buildGoModule {
  pname = "ethsign";
  version = "0.17.1";

  src = ./.;

  vendorHash = "sha256-LSnwKW79m70RiVv9INJb8oEzDmouJtiqYLHNBD8KAzY=";
  proxyVendor = true;

  meta = {
    homepage = "http://github.com/dapphub/dapptools";
    description = "Make raw signed Ethereum transactions";
    license = [lib.licenses.agpl3];
  };
}
