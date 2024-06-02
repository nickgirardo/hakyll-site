{
  description = "The personal site of Nick Girardo";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
    resume.url = "github:nickgirardo/resume/75eec8e30d7d85a4b4d9bc0664bd2ec0694a0ee9";
    trone.url = "github:nickgirardo/trone/fe21b76b83bad4604f582b4023cd3ea5ff715e32";
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
              src = ./site;
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
        hakyll-site = flake.packages."site:exe:site";
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

          resumePath = resume.outputs.packages.${system}.resume;
          tronePath = trone.outputs.packages.${system}.trone;
          sudokuPath = sudoku.outputs.packages.${system}.sudoku;
          ld4842Path = ld48-42.outputs.packages.${system}.ld4842;
          gridTetrisPath = grid-tetris.outputs.packages.${system}.default;

          preBuildPhase = ''
                # Preparing to copy over externally defined resources
                mkdir site/extern/

                # Resume
                cp $resumePath/dist/resume.pdf site/extern/resume.pdf

                # Trone
                mkdir site/extern/trone/
                (cd $tronePath/dist; cp -r . $OLDPWD/site/extern/trone)

                # Sudoku
                mkdir site/extern/sudoku/
                (cd $sudokuPath/dist; cp -r . $OLDPWD/site/extern/sudoku)

                # Grid Tetris
                mkdir site/extern/grid-tetris/
                (cd $gridTetrisPath/dist; cp -r . $OLDPWD/site/extern/grid-tetris)

                # ld48-42
                mkdir site/extern/ld48-42/
                (cd $ld4842Path/dist; cp -r . $OLDPWD/site/extern/ld48-42)
              '';

          buildPhase = ''
                runHook preBuildPhase

                export LAST_UPDATED="2024-06-02"

                cd site/
                ${hakyll-site}/bin/site build --verbose
                # Return to the directory we started in
                cd ..
              '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -a site/_site/. "$out/dist"
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
