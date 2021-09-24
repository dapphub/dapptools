{ lib, buildGoModule }:

buildGoModule rec {
  pname = "depend";
  version = "0.0.1";

  src = ./.;

  vendorSha256 = "0dl8fafgphj5hxbhvmhhqwj60m56cy67ij38v6rarsg34mgq7sxf";

  meta = {
    homepage = http://github.com/dapphub/dapptools;
    description = "Smart contract build and dependency management";
    license = [lib.licenses.agpl3];
  };
}
