{ pkgs }: with pkgs;
let release = true;
    verbose = true;
    aho_corasick_0_6_3_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "aho-corasick";
      version = "0.6.3";
      sha256 = "1cpqzf6acj8lm06z3f1cg41wn6c2n9l3v49nh0dvimv4055qib6k";
      libName = "aho_corasick";
      crateBin = [ {  name = "aho-corasick-dot"; } ];
      inherit dependencies buildDependencies features release verbose;
    };
    backtrace_0_3_3_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "backtrace";
      version = "0.3.3";
      sha256 = "0invfdxkj85v8zyrjs3amfxjdk2a36x8irq7wq7kny6q49hh8y0z";
      inherit dependencies buildDependencies features release verbose;
    };
    backtrace_sys_0_1_16_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "backtrace-sys";
      version = "0.1.16";
      sha256 = "1cn2c8q3dn06crmnk0p62czkngam4l8nf57wy33nz1y5g25pszwy";
      build = "build.rs";
      inherit dependencies buildDependencies features release verbose;
    };
    cc_1_0_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "cc";
      version = "1.0.1";
      sha256 = "1nyml8lw1vfjk4ajbcfdpw02fxahxs9m9gpkwiqm4lyka26za0ag";
      inherit dependencies buildDependencies features release verbose;
    };
    cfg_if_0_1_2_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "cfg-if";
      version = "0.1.2";
      sha256 = "0x06hvrrqy96m97593823vvxcgvjaxckghwyy2jcyc8qc7c6cyhi";
      inherit dependencies buildDependencies features release verbose;
    };
    dbghelp_sys_0_2_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "dbghelp-sys";
      version = "0.2.0";
      sha256 = "0ylpi3bbiy233m57hnisn1df1v0lbl7nsxn34b0anzsgg440hqpq";
      libName = "dbghelp";
      build = "build.rs";
      inherit dependencies buildDependencies features release verbose;
    };
    docopt_0_8_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "docopt";
      version = "0.8.1";
      sha256 = "0kmqy534qgcc2hh81nd248jmnvdjb5y4wclddd7y2jjm27rzibss";
      crateBin = [ {  name = "docopt-wordlist";  path = "src/wordlist.rs"; } ];
      inherit dependencies buildDependencies features release verbose;
    };
    dtoa_0_4_2_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "dtoa";
      version = "0.4.2";
      sha256 = "1bxsh6fags7nr36vlz07ik2a1rzyipc8x1y30kjk832hf2pzadmw";
      inherit dependencies buildDependencies features release verbose;
    };
    error_chain_0_11_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "error-chain";
      version = "0.11.0";
      sha256 = "19nz17q6dzp0mx2jhh9qbj45gkvvgcl7zq9z2ai5a8ihbisfj6d7";
      inherit dependencies buildDependencies features release verbose;
    };
    ethabi_4_1_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "ethabi";
      version = "4.1.0";
      src = ./ethabi;
      inherit dependencies buildDependencies features release verbose;
    };
    ethabi_cli_4_0_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "ethabi-cli";
      version = "4.0.0";
      src = ./cli;
      crateBin = [ {  name = "ethabi";  path = "src/main.rs"; } ];
      inherit dependencies buildDependencies features release verbose;
    };
    ethabi_contract_4_1_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "ethabi-contract";
      version = "4.1.0";
      src = ./contract;
      inherit dependencies buildDependencies features release verbose;
    };
    ethabi_derive_4_1_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "ethabi-derive";
      version = "4.1.0";
      src = ./derive;
      procMacro = true;
      inherit dependencies buildDependencies features release verbose;
    };
    ethabi_tests_0_1_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "ethabi-tests";
      version = "0.1.0";
      src = ./.;
      inherit dependencies buildDependencies features release verbose;
    };
    heck_0_2_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "heck";
      version = "0.2.1";
      sha256 = "16156shpigdbz1kkykiv0ddsigg4x0571h4psgrfrfbci5h4dcba";
      inherit dependencies buildDependencies features release verbose;
    };
    itoa_0_3_4_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "itoa";
      version = "0.3.4";
      sha256 = "1nfkzz6vrgj0d9l3yzjkkkqzdgs68y294fjdbl7jq118qi8xc9d9";
      inherit dependencies buildDependencies features release verbose;
    };
    kernel32_sys_0_2_2_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "kernel32-sys";
      version = "0.2.2";
      sha256 = "1lrw1hbinyvr6cp28g60z97w32w8vsk6pahk64pmrv2fmby8srfj";
      libName = "kernel32";
      build = "build.rs";
      inherit dependencies buildDependencies features release verbose;
    };
    lazy_static_0_2_9_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "lazy_static";
      version = "0.2.9";
      sha256 = "08ldzr5292y3hvi6l6v8l4i6v95lm1aysmnfln65h10sqrfh6iw7";
      inherit dependencies buildDependencies features release verbose;
    };
    libc_0_2_32_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "libc";
      version = "0.2.32";
      sha256 = "1i8njlar6v9qvmkyfvwzhxrvkqw6ijp8fqdnya5csqixxz18a532";
      inherit dependencies buildDependencies features release verbose;
    };
    memchr_1_0_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "memchr";
      version = "1.0.1";
      sha256 = "071m5y0zm9p1k7pzqm20f44ixvmycf71xsrpayqaypxrjwchnkxm";
      inherit dependencies buildDependencies features release verbose;
    };
    num_traits_0_1_40_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "num-traits";
      version = "0.1.40";
      sha256 = "1fr8ghp4i97q3agki54i0hpmqxv3s65i2mqd1pinc7w7arc3fplw";
      inherit dependencies buildDependencies features release verbose;
    };
    quote_0_3_15_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "quote";
      version = "0.3.15";
      sha256 = "09il61jv4kd1360spaj46qwyl21fv1qz18fsv2jra8wdnlgl5jsg";
      inherit dependencies buildDependencies features release verbose;
    };
    regex_0_2_2_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "regex";
      version = "0.2.2";
      sha256 = "1f1zrrynfylg0vcfyfp60bybq4rp5g1yk2k7lc7fyz7mmc7k2qr7";
      inherit dependencies buildDependencies features release verbose;
    };
    regex_syntax_0_4_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "regex-syntax";
      version = "0.4.1";
      sha256 = "01yrsm68lj86ad1whgg1z95c2pfsvv58fz8qjcgw7mlszc0c08ls";
      inherit dependencies buildDependencies features release verbose;
    };
    rustc_demangle_0_1_5_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "rustc-demangle";
      version = "0.1.5";
      sha256 = "096kkcx9j747700fhxj1s4rlwkj21pqjmvj64psdj6bakb2q13nc";
      inherit dependencies buildDependencies features release verbose;
    };
    rustc_hex_1_0_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "rustc-hex";
      version = "1.0.0";
      sha256 = "1rvrll1vmsdi09bq4j03vvc44kh92174kq1gkxdiwpc3d41l1r9i";
      inherit dependencies buildDependencies features release verbose;
    };
    serde_1_0_15_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "serde";
      version = "1.0.15";
      sha256 = "0pj4qq0is7abcd1jw0q66lw1q583rxljmjrriic7v1i2m5fardq2";
      inherit dependencies buildDependencies features release verbose;
    };
    serde_derive_1_0_15_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "serde_derive";
      version = "1.0.15";
      sha256 = "15zp8gp0h8962z40xdzay83p5kd55s24nwhdcp6ab10963lb9blk";
      procMacro = true;
      inherit dependencies buildDependencies features release verbose;
    };
    serde_derive_internals_0_16_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "serde_derive_internals";
      version = "0.16.0";
      sha256 = "1k96ypwlhnvmaksimkx1pd5rwvjaanfcdzpgndhy994hx03xplhs";
      inherit dependencies buildDependencies features release verbose;
    };
    serde_json_1_0_4_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "serde_json";
      version = "1.0.4";
      sha256 = "174cn8v7x42phmd789wsqvw9b1idmpfcpxcbp00pwhnb8l2i6lin";
      inherit dependencies buildDependencies features release verbose;
    };
    strsim_0_6_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "strsim";
      version = "0.6.0";
      sha256 = "1lz85l6y68hr62lv4baww29yy7g8pg20dlr0lbaswxmmcb0wl7gd";
      inherit dependencies buildDependencies features release verbose;
    };
    syn_0_11_11_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "syn";
      version = "0.11.11";
      sha256 = "0yw8ng7x1dn5a6ykg0ib49y7r9nhzgpiq2989rqdp7rdz3n85502";
      inherit dependencies buildDependencies features release verbose;
    };
    synom_0_11_3_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "synom";
      version = "0.11.3";
      sha256 = "1l6d1s9qjfp6ng2s2z8219igvlv7gyk8gby97sdykqc1r93d8rhc";
      inherit dependencies buildDependencies features release verbose;
    };
    thread_local_0_3_4_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "thread_local";
      version = "0.3.4";
      sha256 = "1y6cwyhhx2nkz4b3dziwhqdvgq830z8wjp32b40pjd8r0hxqv2jr";
      inherit dependencies buildDependencies features release verbose;
    };
    tiny_keccak_1_3_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "tiny-keccak";
      version = "1.3.1";
      sha256 = "0sf70d2yq2nb8rxlvjh779lv4xkfb0zwmgmvkqd3ala7grxn6dbh";
      inherit dependencies buildDependencies features release verbose;
    };
    unicode_segmentation_1_2_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "unicode-segmentation";
      version = "1.2.0";
      sha256 = "0yz43x7wrhr3n7a2zsinx3r60yxsdqicg8a5kycyyhdaq1zmiz1y";
      inherit dependencies buildDependencies features release verbose;
    };
    unicode_xid_0_0_4_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "unicode-xid";
      version = "0.0.4";
      sha256 = "1dc8wkkcd3s6534s5aw4lbjn8m67flkkbnajp5bl8408wdg8rh9v";
      inherit dependencies buildDependencies features release verbose;
    };
    unreachable_1_0_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "unreachable";
      version = "1.0.0";
      sha256 = "1am8czbk5wwr25gbp2zr007744fxjshhdqjz9liz7wl4pnv3whcf";
      inherit dependencies buildDependencies features release verbose;
    };
    utf8_ranges_1_0_0_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "utf8-ranges";
      version = "1.0.0";
      sha256 = "0rzmqprwjv9yp1n0qqgahgm24872x6c0xddfym5pfndy7a36vkn0";
      inherit dependencies buildDependencies features release verbose;
    };
    void_1_0_2_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "void";
      version = "1.0.2";
      sha256 = "0h1dm0dx8dhf56a83k68mijyxigqhizpskwxfdrs1drwv2cdclv3";
      inherit dependencies buildDependencies features release verbose;
    };
    winapi_0_2_8_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "winapi";
      version = "0.2.8";
      sha256 = "0a45b58ywf12vb7gvj6h3j264nydynmzyqz8d8rqxsj6icqv82as";
      inherit dependencies buildDependencies features release verbose;
    };
    winapi_build_0_1_1_ = { dependencies?[], buildDependencies?[], features?[] }: buildRustCrate {
      crateName = "winapi-build";
      version = "0.1.1";
      sha256 = "1lxlpi87rkhxcwp2ykf1ldw3p108hwm24nywf3jfrvmff4rjhqga";
      libName = "build";
      inherit dependencies buildDependencies features release verbose;
    };

in
rec {
  aho_corasick_0_6_3 = aho_corasick_0_6_3_ {
    dependencies = [ memchr_1_0_1 ];
  };
  backtrace_0_3_3 = backtrace_0_3_3_ {
    dependencies = [ cfg_if_0_1_2 rustc_demangle_0_1_5 ]
      ++ (if (buildPlatform.parsed.kernel.name == "linux" || buildPlatform.parsed.kernel.name == "darwin") && !(buildPlatform.parsed.kernel.name == "emscripten") && !(buildPlatform.parsed.kernel.name == "darwin") && !(buildPlatform.parsed.kernel.name == "ios") then [ backtrace_sys_0_1_16 ] else [])
      ++ (if (buildPlatform.parsed.kernel.name == "linux" || buildPlatform.parsed.kernel.name == "darwin") then [ libc_0_2_32 ] else [])
      ++ (if buildPlatform.parsed.kernel.name == "windows" then [ dbghelp_sys_0_2_0 kernel32_sys_0_2_2 winapi_0_2_8 ] else []);
    features = [ "backtrace-sys" "coresymbolication" "dbghelp" "dbghelp-sys" "dladdr" "kernel32-sys" "libbacktrace" "libunwind" "winapi" ];
  };
  backtrace_sys_0_1_16 = backtrace_sys_0_1_16_ {
    dependencies = [ libc_0_2_32 ];
    buildDependencies = [ cc_1_0_1 ];
  };
  cc_1_0_1 = cc_1_0_1_ {
    dependencies = [];
  };
  cfg_if_0_1_2 = cfg_if_0_1_2_ {};
  dbghelp_sys_0_2_0 = dbghelp_sys_0_2_0_ {
    dependencies = [ winapi_0_2_8 ];
    buildDependencies = [ winapi_build_0_1_1 ];
  };
  docopt_0_8_1 = docopt_0_8_1_ {
    dependencies = [ lazy_static_0_2_9 regex_0_2_2 serde_1_0_15 serde_derive_1_0_15 strsim_0_6_0 ];
  };
  dtoa_0_4_2 = dtoa_0_4_2_ {};
  error_chain_0_11_0 = error_chain_0_11_0_ {
    dependencies = [ backtrace_0_3_3 ];
    features = [ "backtrace" "example_generated" ];
  };
  ethabi_4_1_0 = ethabi_4_1_0_ {
    dependencies = [ error_chain_0_11_0 rustc_hex_1_0_0 serde_1_0_15 serde_derive_1_0_15 serde_json_1_0_4 tiny_keccak_1_3_1 ];
  };
  ethabi_cli_4_0_0 = ethabi_cli_4_0_0_ {
    dependencies = [ docopt_0_8_1 error_chain_0_11_0 ethabi_4_1_0 rustc_hex_1_0_0 serde_1_0_15 serde_derive_1_0_15 ];
  };
  ethabi_contract_4_1_0 = ethabi_contract_4_1_0_ {};
  ethabi_derive_4_1_0 = ethabi_derive_4_1_0_ {
    dependencies = [ ethabi_4_1_0 heck_0_2_1 quote_0_3_15 syn_0_11_11 ];
  };
  ethabi_tests_0_1_0 = ethabi_tests_0_1_0_ {
    dependencies = [ ethabi_4_1_0 ethabi_contract_4_1_0 ethabi_derive_4_1_0 rustc_hex_1_0_0 ];
  };
  heck_0_2_1 = heck_0_2_1_ {
    dependencies = [ unicode_segmentation_1_2_0 ];
  };
  itoa_0_3_4 = itoa_0_3_4_ {};
  kernel32_sys_0_2_2 = kernel32_sys_0_2_2_ {
    dependencies = [ winapi_0_2_8 ];
    buildDependencies = [ winapi_build_0_1_1 ];
  };
  lazy_static_0_2_9 = lazy_static_0_2_9_ {
    dependencies = [];
  };
  libc_0_2_32 = libc_0_2_32_ {};
  memchr_1_0_1 = memchr_1_0_1_ {
    dependencies = [ libc_0_2_32 ];
  };
  num_traits_0_1_40 = num_traits_0_1_40_ {};
  quote_0_3_15 = quote_0_3_15_ {};
  regex_0_2_2 = regex_0_2_2_ {
    dependencies = [ aho_corasick_0_6_3 memchr_1_0_1 regex_syntax_0_4_1 thread_local_0_3_4 utf8_ranges_1_0_0 ];
  };
  regex_syntax_0_4_1 = regex_syntax_0_4_1_ {};
  rustc_demangle_0_1_5 = rustc_demangle_0_1_5_ {};
  rustc_hex_1_0_0 = rustc_hex_1_0_0_ {};
  serde_1_0_15 = serde_1_0_15_ {
    features = [ "std" ];
  };
  serde_derive_1_0_15 = serde_derive_1_0_15_ {
    dependencies = [ quote_0_3_15 serde_derive_internals_0_16_0 syn_0_11_11 ];
  };
  serde_derive_internals_0_16_0 = serde_derive_internals_0_16_0_ {
    dependencies = [ syn_0_11_11 synom_0_11_3 ];
  };
  serde_json_1_0_4 = serde_json_1_0_4_ {
    dependencies = [ dtoa_0_4_2 itoa_0_3_4 num_traits_0_1_40 serde_1_0_15 ];
  };
  strsim_0_6_0 = strsim_0_6_0_ {};
  syn_0_11_11 = syn_0_11_11_ {
    dependencies = [ quote_0_3_15 synom_0_11_3 unicode_xid_0_0_4 ];
    features = [ "parsing" "printing" "quote" "synom" "unicode-xid" "visit" ];
  };
  synom_0_11_3 = synom_0_11_3_ {
    dependencies = [ unicode_xid_0_0_4 ];
  };
  thread_local_0_3_4 = thread_local_0_3_4_ {
    dependencies = [ lazy_static_0_2_9 unreachable_1_0_0 ];
  };
  tiny_keccak_1_3_1 = tiny_keccak_1_3_1_ {};
  unicode_segmentation_1_2_0 = unicode_segmentation_1_2_0_ {};
  unicode_xid_0_0_4 = unicode_xid_0_0_4_ {};
  unreachable_1_0_0 = unreachable_1_0_0_ {
    dependencies = [ void_1_0_2 ];
  };
  utf8_ranges_1_0_0 = utf8_ranges_1_0_0_ {};
  void_1_0_2 = void_1_0_2_ {};
  winapi_0_2_8 = winapi_0_2_8_ {};
  winapi_build_0_1_1 = winapi_build_0_1_1_ {};
}
