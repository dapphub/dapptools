{ pkgs }:

let inherit (import ./build-dapp.nix { inherit pkgs; })
  dappsysPackage;

in rec {
  dappsys = rec {

    ##################################################################
    #                          Standards                             #
    ##################################################################

    erc20 = dappsysPackage {
      name = "erc20";
      rev = "ac319b25d7f63f90cb1b47c3bb0b91f586d917bf";
      sha256 = "1cwzki009882fw00lx3a2x3qrp5vs13hh9k6h6by15mny7i1ynxb";
    };

    ##################################################################
    #                       Dappsys mixins                           #
    ##################################################################

    ds-auth = dappsysPackage {
      name = "ds-auth";
      deps = [ds-test];
      rev = "c0050bbb6807027c623b1a1ee7afd86515cdb004";
      sha256 = "08a544lmikaqz87alw67286rv31bxv8hmwgmhz7a2pxw25d0xxbj";
    };

    ds-exec = dappsysPackage {
      name = "ds-exec";
      deps = [ds-test];
      rev = "2e2255dd657da2d37e136cbb366f9dddef20b986";
      sha256 = "1c7n2gvbqpl2b5fy3q1anx1k37m56jz81792kapc1llsyqhxbsn9";
    };

    ds-math = dappsysPackage {
      name = "ds-math";
      deps = [ds-test];
      rev = "3f24d6b228bb65ddbbd3a54b4b6b55e9d17e98b3";
      sha256 = "0aqixy091fbzm2gnzr9qzwh7k27l4vcxgm2kknm7995yz7cn4fdj";
    };

    ds-note = dappsysPackage {
      name = "ds-note";
      deps = [ds-test];
      rev = "8db3300a9ff4d779a2861255c97dfb89a090e1c9";
      sha256 = "0iym11478znghadzl10c88kv4pvkqpbjlar0a02mz8idn7864zgc";
    };

    ds-stop = dappsysPackage {
      name = "ds-stop";
      deps = [ds-test ds-auth ds-note];
      rev = "87191458553cdd39514708082d3a53cb20f0e811";
      sha256 = "0k21if5c65pfmfi8h76r1fsgh5fsf64lqcasr5jccjcs07w83swc";
    };

    ds-test = dappsysPackage {
      name = "ds-test";
      rev = "8053e2589749238451678b5a75028bc830dc31cb";
      sha256 = "0ifxf6m1q4i0rlg46yh9a8brsyj8m0bybs2gx4sx6kd5300b1qjq";
    };

    ds-warp = dappsysPackage {
      name = "ds-warp";
      deps = [ds-note ds-test];
      rev = "22446c760580c60710fddee9e8e7f335b8966db4";
      sha256 = "10r06qbl0xk6y2wgkx4g0hy5f9ln94793daair2pgzdaknx0aqx7";
    };

    ##################################################################
    #                       Dappsys boxes                            #
    ##################################################################

    ds-cache = dappsysPackage {
      name = "ds-cache";
      deps = [ds-test ds-value];
      rev = "56ffdf4c5610adf5916eccd274e1b006930cb6f7";
      sha256 = "1h4bx80y076bvf2mmb5nkzjaypwicnbhdnliva8gw04wnfbkf85j";
    };

    ds-chief = dappsysPackage {
      name = "ds-chief";
      deps = [ds-roles ds-test ds-thing ds-token];
      rev = "d05ee2b80b7e9c069a1d7b981415e7fa33423bfe";
      sha256 = "0icxjhn9y97mhn764m2pqghwv7kagm47zbmxajrcliy4bzl041ic";
    };

    ds-group = dappsysPackage {
      name = "ds-group";
      deps = [ds-exec ds-note ds-test];
      rev = "00aeb89b684528f433ecb0ea6015317af68bcdac";
      sha256 = "00232fdds9gv71q044j3y37mmi7drm7lsz37cqvkr31zgygck2hf";
    };

    ds-guard = dappsysPackage {
      name = "ds-guard";
      deps = [ds-auth ds-test];
      rev = "ce2731de69711e825516ef070be551a7267b073e";
      sha256 = "1dzyf8zpsiyhwc7kx8ns812ijy94qk5llvpw1wb1cxxmwy8vfhf8";
    };

    ds-proxy = dappsysPackage {
      name = "ds-proxy";
      deps = [ds-auth ds-note ds-test];
      rev = "e257e3f672df234a785dd4a82175b622adaed55b";
      sha256 = "0ylxyd71znxaqdwpf0xdqqzn57x0krqn55jzdy9ynwm5nly75i8w";
    };

    ds-roles = dappsysPackage {
      name = "ds-roles";
      deps = [ds-auth ds-test];
      rev = "a249d9376f5d70dc3e3cb5933005e232e0e90fe3";
      sha256 = "11yhgv0vz4wgypdqax2cv2v4njwabnrji3iy33hq5aich892krfi";
    };

    ds-thing = dappsysPackage {
      name = "ds-thing";
      deps = [ds-auth ds-math ds-note ds-test];
      rev = "66e028b927a6327436090ec5bb98b7c0654ff422";
      sha256 = "0rija8bk88chgyzxv4f7xckqxnzk6f30mdbhks1jqpy5gssy3cq5";
    };

    ds-token = dappsysPackage {
      name = "ds-token";
      deps = [ds-test erc20 ds-math ds-stop];
      rev = "07a68ed271ea051f23dc063bd78a4966dc04ae5b";
      sha256 = "1k3rhxvll5p1lq251z6fdnla0pr72kh7ncl4hlm3a42hbb8qx66i";
    };

    ds-value = dappsysPackage {
      name = "ds-value";
      deps = [ds-test ds-thing];
      rev = "a6e658e90a053f977a83772169efd583639f4c99";
      sha256 = "0pwci0x7xsy4wv3yxihc7pm3i63q38xlr7532c6m9ygbrrmdwnbg";
    };

    ds-vault = dappsysPackage {
      name = "ds-vault";
      deps = [ds-test ds-token];
      rev = "b703c6c1fd1eb8732c08a52e1caaaa3677c46ac6";
      sha256 = "1y0h5kyx3983g30fmp2vns6jhin60wx633c154brc69w8wqchpm9";
    };
  };

  dummy-dappsys = pkgs.runCommand "dummy" {
    buildInputs = builtins.attrValues dappsys;
  } "";
}
