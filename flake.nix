{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    resume.url = "github:nickgirardo/resume/6c15624737a6728cef98ccda8ceaf8efd951cab7";
    trone.url = "github:nickgirardo/trone/dfc7629ee86be4bed705f17b92d028abbf9c2fb4";
    sudoku.url = "github:nickgirardo/sudoku/eb40b3464d884c2b64c340fc1052577c06916967";
    ld48-42.url = "github:nickgirardo/ld48-42/ff194c48a9127712651b4c25e6bda3a8d91873e8";
    grid-tetris.url = "github:nickgirardo/grid-tetris/9547afb3eecaf939bbb93685a48c583238cf5367";
  };


  outputs = { self, nixpkgs, flake-utils, haskellNix, resume, trone, sudoku, ld48-42, grid-tetris }:
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
          sudokuLoc = sudoku.outputs.packages.${system}.sudoku;
          ld4842Loc = ld48-42.outputs.packages.${system}.ld4842;
          gridTetrisLoc = grid-tetris.outputs.packages.${system}.default;

          preBuildPhase = ''
                # Preparing to copy over externally defined resources
                mkdir my-site/extern/

                # Resume
                cp $resumeLoc/dist/resume.pdf my-site/extern/resume.pdf

                # Trone
                mkdir my-site/extern/trone/
                (cd $troneLoc/dist; cp -r . $OLDPWD/my-site/extern/trone)

                # Sudoku
                mkdir my-site/extern/sudoku/
                (cd $sudokuLoc/dist; cp -r . $OLDPWD/my-site/extern/sudoku)

                # Grid Tetris
                mkdir my-site/extern/grid-tetris/
                (cd $gridTetrisLoc/dist; cp -r . $OLDPWD/my-site/extern/grid-tetris)

                # ld48-42
                mkdir my-site/extern/ld48-42/
                (cd $ld4842Loc/dist; cp -r . $OLDPWD/my-site/extern/ld48-42)
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
        apps = let
          serv = pkgs.writeShellApplication {
            # Our shell script name is serve so it is available at $out/bin/serve
            name = "serve";
            runtimeInputs = [pkgs.caddy];
            text = ''
              # Takes the port as first argument
              # Default to port 8080
              PORT="''${1:-8080}"
              caddy file-server --listen :"$PORT" --root ${website.outPath}/dist
            '';
          };
          serve = {
            type = "app";
            program = "${serv}/bin/serve";
          };
        in {
          inherit serve;
          default = serve;
        };

        packages = {
          inherit hakyll-site website;
          default = website;
        };
      });
}
