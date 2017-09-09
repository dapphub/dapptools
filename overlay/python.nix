{ pkgs }: self: super: with super;

let
  stdenv = pkgs.stdenv;
  fetchPypi = pkgs.pythonPackages.fetchPypi;

in rec {

  web3 = buildPythonPackage rec {
    pname = "web3";
    version = "3.13.5";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "0q27bqgy4a847spx4zarg6qahvc0a38dqr1pgwlj8dnh20qp2pf3";
    };

    propagatedBuildInputs = with self; [ethereum-abi-utils requests];
    doCheck = false;

    meta = with stdenv.lib; {
      homepage = https://github.com/pipermerriam/web3.py;
      description = "A Python interface for the Ethereum blockchain";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  ethereum = buildPythonPackage rec {
    pname = "ethereum";
    version = "1.6.1";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "0k93zs01ff7ki835c3f7gp2sskhr3xaiqm1mkn60i60kyipslfrc";
    };
    preConfigure = ''
      substituteInPlace setup.py --replace pytest-runner==2.7 pytest-runner==2.6.2
    '';
    propagatedBuildInputs = with self; [
      pyyaml rlp pysha3 pyethash bitcoin pbkdf2 repoze_lru scrypt
      pycryptodome pytestrunner secp256k1
    ];
    doCheck = false;
    meta = with stdenv.lib; {
      homepage = https://github.com/ethereum/pyethereum;
      description = "Python core library of the Ethereum project";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  scrypt = buildPythonPackage rec {
    pname = "scrypt";
    version = "0.8.0";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "0830r3q8f8mc4738ngcvwhv9kih5c6zf87mzkdifzf2h6kss99fl";
    };
    buildInputs = [pkgs.openssl];
    doCheck = false;
    meta = with stdenv.lib; {
      homepage = https://bitbucket.org/mhallin/py-scrypt/src;
      description = "Bindings for the scrypt key derivation function";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  bitcoin = buildPythonPackage rec {
    pname = "bitcoin";
    version = "1.1.42";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "0dkjifd8d60an0jl6k9zqx9r0p5xprzlrgf4n9mlyxhwksyp1fhi";
    };
    propagatedBuildInputs = with self; [];
    meta = with stdenv.lib; {
      homepage = https://github.com/vbuterin/pybitcointools;
      description = "Bitcoin-themed Python ECC library";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  pyethash = buildPythonPackage rec {
    pname = "pyethash";
    version = "0.1.27";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "0618kkn2sb0a3h2pphpj2vi455xc9mil42b13zgpg7bbwaf32rpz";
    };
    propagatedBuildInputs = with self; [];
    meta = with stdenv.lib; {
      homepage = https://github.com/ethereum/ethash;
      description = "Python wrappers for the Ethereum PoW hash function";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  ethereum-abi-utils = buildPythonPackage rec {
    pname = "ethereum-abi-utils";
    version = "0.4.0";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "1170dxm8vffpj4gc2mmpndj61mrwmsp3y2xnfhda1rj5w33bm7js";
    };

    checkInputs = with self; [pytest];
    propagatedBuildInputs = with self; [ethereum-utils];

    meta = with stdenv.lib; {
      homepage = https://github.com/pipermerriam/ethereum-abi-utils;
      description = "Ethereum ABI utilities for Python";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  ethereum-utils = buildPythonPackage rec {
    pname = "ethereum-utils";
    version = "0.4.0";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "1z8bpkwmfrn6rrigippydkpl4077r5g81h3vrmdy4ri8iy5519rz";
    };

    checkInputs = with self; [pytest];
    propagatedBuildInputs = with self; [pysha3 cytoolz pylru rlp];

    meta = with stdenv.lib; {
      homepage = https://github.com/pipermerriam/ethereum-utils;
      description = "Ethereum utilities for Python";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  pysha3 = buildPythonPackage rec {
    pname = "pysha3";
    version = "1.0.2";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "17kkjapv6sr906ib0r5wpldmzw7scza08kv241r98vffy9rqx67y";
    };

    checkInputs = with self; [pytest];

    meta = with stdenv.lib; {
      homepage = https://github.com/tiran/pysha3;
      description = "SHA-3 wrapper (Keccak) for Python";
      license = licenses.psfl;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  rlp = buildPythonPackage rec {
    pname = "rlp";
    version = "0.6.0";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "0d3gx4mp8q4z369s5yk1n9c55sgfw9fidbwqxq67d6s7l45rm1w7";
    };

    checkInputs = [self.pytest];
    propagatedBuildInputs = with self; [wheel];

    meta = with stdenv.lib; {
      homepage = https://github.com/ethereum/pyrlp;
      description = "Recursive length prefix notation for Ethereum";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  eth-testrpc = buildPythonPackage rec {
    pname = "eth-testrpc";
    version = "1.3.0";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "1bmds2shkxxvhdksji1dxiadm95rf6f8cp7bzs0iirjrx1fyjni0";
    };

    doCheck = false;
    propagatedBuildInputs = with self; [werkzeug click rlp json-rpc ethereum];

    meta = with stdenv.lib; {
      homepage = https://github.com/pipermerriam/eth-testrpc;
      description = "Used for testing Ethereum JSON-RPC interactions";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  json-rpc = buildPythonPackage rec {
    pname = "json-rpc";
    version = "1.10.3";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "1195767r25mclnkz1pxr74wm0j21qqyq75pkw85fsxf9d8wj8gni";
    };
    doCheck = false; # Installs too many web servers
    meta = with stdenv.lib; {
      homepage = https://github.com/pavlov99/json-rpc;
      description = "JSON-RPC2.0 and JSON-RPC1.0 transport specification implementation";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };

  tinydb = buildPythonPackage rec {
    pname = "tinydb";
    version = "3.5.0";
    name = "${pname}-${version}";
    src = fetchPypi {
      inherit pname version;
      sha256 = "18rwlyqbsp7g5pjq792w2vc4gwbb281lgfjvfrg4idlhir53klh0";
    };

    checkInputs = [self.pytest];

    meta = with stdenv.lib; {
      homepage = https://github.com/msiemens/tinydb;
      description = "Pure Python lightweight document oriented database";
      license = licenses.mit;
      maintainers = with maintainers; [ dbrock ];
    };
  };
}
