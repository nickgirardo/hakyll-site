{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    resume.url = "github:nickgirardo/resume/6c15624737a6728cef98ccda8ceaf8efd951cab7";
    trone.url = "github:nickgirardo/trone/ade643ae500ace303959d0319c757811ba46b4bb";
  };


  outputs = { self, nixpkgs, flake-utils, haskellNix, resume, trone }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            hakyllProject = final.haskell-nix.project' {
              compiler-nix-name = "ghc948";
              src = ./my-site;
              shell.buildInputs = [
                hakyll-site
              ];
              shell.tools = {
                cabal = "latest";
                hlint = "latest";
                haskell-language-server = "latest";
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hakyllProject.flake {};
        hakyll-site = flake.packages."my-site:exe:site";
        website = pkgs.stdenv.mkDerivation {
          name = "website";
          buildInputs = [];
          # TODO filter src here
          src = ./.;
          # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
          #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = pkgs.lib.optionalString
            (pkgs.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          resumeLoc = resume.outputs.packages.${system}.resume;
          troneLoc = trone.outputs.packages.${system}.trone;

          preBuildPhase = ''
                # Preparing to copy over externally defined resources
                mkdir my-site/extern/

                # Resume
                cp $resumeLoc/dist/resume.pdf my-site/extern/resume.pdf

                # Trone
                mkdir my-site/extern/trone/
                (cd $troneLoc/dist; cp -r . $OLDPWD/my-site/extern/trone)
              '';

          buildPhase = ''
                runHook preBuildPhase

                cd my-site/
                ${hakyll-site}/bin/site build --verbose
                # Return to the directory we started in
                cd ..
              '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -a my-site/_site/. "$out/dist"
          '';
        };
      in flake // {
        apps.default = flake-utils.lib.mkApp {
          drv = hakyll-site;
          exePath = "/bin/site";
        };

        packages = {
          inherit hakyll-site website;
          default = website;
        };
      });
}
