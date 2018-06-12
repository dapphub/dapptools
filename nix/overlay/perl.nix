# These entries were generated using nix-generate-from-cpan.

{ buildPerlPackage, perlPackages }: with perlPackages; rec {

  GitFastExport = buildPerlPackage rec {
    name = "Git-FastExport-0.107";
    src = fetchurl {
      url = "mirror://cpan/authors/id/B/BO/BOOK/${name}.tar.gz";
      sha256 = "8607a8d7ea2c46f8cb78199b2f584cc1cf1d97dbf2cef4e74e23076c9095f89a";
    };
    buildInputs = [ TestRequiresGit TestScript ];
    propagatedBuildInputs = [ GitRepository ];
    meta = {
      description = "A parser for git fast-export streams";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  SystemCommand = buildPerlPackage rec {
    name = "System-Command-1.118";
    src = fetchurl {
      url = "mirror://cpan/authors/id/B/BO/BOOK/${name}.tar.gz";
      sha256 = "00c4dcc606aeea4d5d58ca027bfb484a7f59edfca882c84d01924c2af32f1ea7";
    };
    propagatedBuildInputs = [ IPCRun ];
    meta = {
      description = "Object for running system commands";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  TestRequiresGit = buildPerlPackage rec {
    name = "Test-Requires-Git-1.008";
    src = fetchurl {
      url = "mirror://cpan/authors/id/B/BO/BOOK/${name}.tar.gz";
      sha256 = "70916210970d84d7491451159ab8b67e15251c8c0dae7c3df6c8d88542ea42a6";
    };
    propagatedBuildInputs = [ GitVersionCompare ];
    meta = {
      description = "Check your test requirements against the available version of Git";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  GitVersionCompare = buildPerlPackage rec {
    name = "Git-Version-Compare-1.004";
    src = fetchurl {
      url = "mirror://cpan/authors/id/B/BO/BOOK/${name}.tar.gz";
      sha256 = "63e8264ed351cb2371b47852a72366214164b5f3fad9dbd68309c7fc63d06491";
    };
    buildInputs = [ TestNoWarnings ];
    meta = {
      description = "Functions to compare Git versions";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };

  GitRepository = buildPerlPackage rec {
    name = "Git-Repository-1.321";
    src = fetchurl {
      url = "mirror://cpan/authors/id/B/BO/BOOK/${name}.tar.gz";
      sha256 = "67848f2dfc09adb208e21fc6fcaf55fc4e3b0f6e687b8e866d4c87520701880c";
    };
    buildInputs = [ TestRequiresGit ];
    propagatedBuildInputs = [ GitVersionCompare SystemCommand namespaceclean ];
    meta = {
      description = "Perl interface to Git repositories";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };
}
