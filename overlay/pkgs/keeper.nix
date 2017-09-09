{ stdenv, fetchFromGitHub, python3Packages, openssl }:

with python3Packages;

buildPythonApplication rec {
  rev = "mbrock";
  version = rev;
  name = "keeper";

  src = fetchFromGitHub {
    inherit rev;
    owner = "makerdao";
    repo = "keeper";
    sha256 = "1yxgxs5iazxbsq1wlj0vhvplid207590nivga5hrwqr4ahy0cs4y";
  };

  propagatedBuildInputs = [
    pytest web3 eth-testrpc sortedcontainers networkx tinydb
  ];

  doCheck = false;

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/${python.sitePackages}
    cp -r keeper $out/${python.sitePackages}
    cp bin/* $out/bin
  '';

  fixupPhase = ''
    for x in $out/bin/*; do wrapProgram "$x" \
      --set PYTHONPATH "$PYTHONPATH:$out/${python.sitePackages}" \
      --set PATH ${python}/bin:$PATH
    done
  '';

  meta = with stdenv.lib; {
    description = "Maker Keeper framework";
    homepage = https://github.com/makerdao/keeper;
    license = licenses.agpl3;
    platforms = platforms.unix;
    maintainers = with maintainers; [ mbrock ];
  };
}
