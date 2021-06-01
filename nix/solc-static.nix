{path, version, sha256}:

{stdenv, fetchurl, lib, pkgs, autoPatchelfHook}:

stdenv.mkDerivation rec {
  pname = "solc-static";
  inherit version;

  platform =
    if lib.strings.hasPrefix "solc-linux-amd64" "${path}"
    then "linux-amd64"
    else "macosx-amd64";

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  solc = fetchurl {
    url = "https://binaries.soliditylang.org/${platform}/${path}";
    sha256 = "${sha256}";
  };

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ pkgs.z3.lib ];

  installPhase = ''
    mkdir -p $out/bin
    install ${solc} $out/bin/solc-${version}
    chmod +x $out/bin/solc-${version}
  '';
}
