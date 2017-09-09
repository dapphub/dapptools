{ stdenv, makeWrapper, lib, fetchFromGitHub
, seth, curl, jshon
}:

stdenv.mkDerivation rec {
  name = "setzer-${version}";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "makerdao";
    repo = "setzer";
    rev = "5d2d31dcdd48715107412bbd280e51d401344952";
    sha256 = "164vvnhr5bxgzzmynggr4y3yn7x2wl95hlr95dzvmyc2vn1vk01q";
  };

  nativeBuildInputs = [makeWrapper];
  buildPhase = "true";
  makeFlags = ["prefix=$(out)"];
  postInstall = let path = lib.makeBinPath [
    seth curl jshon
  ]; in ''
    wrapProgram "$out/bin/setzer" --prefix PATH : "${path}"
  '';

  meta = {
    description = "Ethereum price feed tool";
    homepage = https://github.com/makerdao/setzer;
    maintainers = [stdenv.lib.maintainers.mbrock];
    license = lib.licenses.gpl3;
    inherit version;
  };
}
