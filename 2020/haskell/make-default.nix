{ src }:
let

  name = "advent-of-code";
  compiler-nix-name = "ghc883";
  fetchNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchNiv "haskell.nix") {
    sourceOverrides = {
      hackage = import (fetchNiv "hackage.nix");
    };
  };

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays
      ++ [
        (pkgsNew: pkgsOld: let inherit (pkgsNew) lib; in {
          haskell-nix = pkgsOld.haskell-nix // {
            hackageSrc = sources.${"hackage.nix"};
            stackageSrc = sources.stackage-nix;
            custom-tools = pkgsOld.haskell-nix.custom-tools // {
              haskell-language-server."0.6.0" = args:
                let
                  project = pkgsOld.haskell-nix.project' (args // {
                    src = pkgsOld.evalPackages.fetchgit {
                      url = "https://github.com/haskell/haskell-language-server.git";
                      fetchSubmodules = true;
                      rev = "372a12e797069dc3ac4fa33dcaabe3b992999d7c";
                      sha256 = "027fq6752024wzzq9izsilm5lkq9gmpxf82rixbimbijw0yk4pwj";
                    };
                    projectFileName = "cabal.project";
                    sha256map = {
                      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
                      "https://github.com/bubba/hie-bios.git"."cec139a1c3da1632d9a59271acc70156413017e7" = "1iqk55jga4naghmh8zak9q7ssxawk820vw8932dhympb767dfkha";
                    };
                    cabalProjectLocal = ''
                      allow-newer: diagrams-svg:base, monoid-extras:base, svg-builder:base,
                        diagrams-lib:base, dual-tree:base, active:base, diagrams-core:base,
                        diagrams-contrib:base, force-layout:base, diagrams-postscript:base,
                        statestack:base
                    '';
                  });
                in
                  pkgsOld.symlinkJoin {
                    name = "haskell-language-server";
                    paths = with (project.hsPkgs.haskell-language-server.components.exes); [
                      haskell-language-server
                      haskell-language-server-wrapper
                    ];
                  };
            };
          };
        })
    ];
  });

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name src;
      subDir = "2020/haskell";
    };

  };

in

set.${name}.components.library // {

  shell = set.shellFor {

    buildInputs =
      [
        pkgs.idris
      ];

    exactDeps = false;

    packages = p: [
      p.${name}
    ];

    tools = {
      cabal = {
        inherit compiler-nix-name;
        version = "3.2.0.0";
      };
      haskell-language-server = "0.6.0";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
