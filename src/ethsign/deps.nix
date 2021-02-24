[
    {
      goPackagePath = "github.com/ethereum/go-ethereum";
      fetch = fetchFromGitHub {
        type = "git";
        url = "https://github.com/ethereum/go-ethereum";
        rev = "v1.9.3";
        sha256 = "0pm8gfr4g7rbax6vzxv6lklpx83mxghah7fyvpk3jqvm1mq299ln";
    };
    }
   # # {
   # #    goPackagePath = "github.com/VictoriaMetrics/fastcache";
   # #    src = fetchFromGitHub {
   # #      owner = "VictoriaMetrics";
   # #      repo = "fastcache";
   # #      rev = "v1.5.7";
   # #      sha256 = "19fsx8ha2kdjbsinlllz70fj96z6xhvn38x8qrmml0abmsf5z60k";
   # #    };
   # # }
   # # {
   # #    goPackagePath = "github.com/aristanetworks/goarista/monotime";
   # #    src = fetchFromGitHub {
   # #      owner = "aristanetworks";
   # #      repo = "goarista";
   # #      rev = "ockafka-v0.0.5";
   # #      sha256 = "0qhmn83cny1299rli2x8n2rw844gfwscsldrnl107vi8mv88r0xw";
   # #    };
   # #  }
   # # {
   # #    goPackagePath = "github.com/cespare/xxhash/v2";
   # #    src = fetchFromGitHub {
   # #      owner = "cespare";
   # #      repo = "xxhash";
   # #      rev = "v2.1.1";
   # #      sha256 = "0rl5rs8546zj1vzggv38w93wx0b5dvav7yy5hzxa8kw7iikv1cgr";
   # #    };
   # #  }
   #  {
   #    goPackagePath = "gopkg.in/urfave/cli.v1";
   #    src = fetchFromGitHub {
   #      owner = "urfave";
   #      repo = "cli";
   #      rev = "v1.19.1";
   #      sha256 = "1ny63c7bfwfrsp7vfkvb4i0xhq4v7yxqnwxa52y4xlfxs4r6v6fg";
   #    };
   #  }
   #  {
   #    goPackagePath = "golang.org/x/crypto";
   #    src = fetchgit {
   #      url = "https://go.googlesource.com/crypto";
   #      rev = "948cd5f35899cbf089c620b3caeac9b60fa08704";
   #      sha256 = "05ym028qsqkh0ba9bwny3g25yaggm0brdwpjd6k3fx5h0aqkiwh1";
   #    };
   #  }
   #  {
   #    goPackagePath = "golang.org/x/sys";
   #    src = fetchgit {
   #      url = "https://go.googlesource.com/sys";
   #      rev = "53aa286056ef226755cd898109dbcdaba8ac0b81";
   #      sha256 = "1yd17ccklby099cpdcsgx6lf0lj968hsnppp16mwh9009ldf72r1";
   #    };
   #  }
]
