{path, version, sha256}:

{stdenv, fetchurl, lib, z3, makeWrapper, autoPatchelfHook}:
let
  # solc uses dlopen to look for z3 at runtime, and expects to find a library
  # called libz3.so.4.8 exactly. The z3.lib provided by nixpkgs only has a
  # libz3.so, so we have to hack around it with this derivation that gives us a
  # copy of libz3.so with the name that solc expects.
  # At some point in the future we're going to need some logic to provide
  # different versions of z3 to different versions of solc, but for now we just
  # give z3-4.8 to every version of solc
  split = lib.strings.splitString "." z3.version;
  z3-exact = stdenv.mkDerivation {
    pname = z3.pname;
    version = z3.version;
    src = null;
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/lib
      cp ${z3.lib}/lib/libz3.so $out/lib/libz3.so.${builtins.elemAt split 0}.${builtins.elemAt split 1}
    '';
  };
in

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

  nativeBuildInputs = [ makeWrapper ] ++ lib.optional stdenv.isLinux autoPatchelfHook;

  postFixup = if (platform == "linux-amd64") then ''
      wrapProgram $out/bin/solc-${version} \
        --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ z3-exact ]}
  '' else "";

  installPhase = ''
    mkdir -p $out/bin
    install ${solc} $out/bin/solc-${version}
    chmod +x $out/bin/solc-${version}
  '';
}
