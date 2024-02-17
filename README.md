# Personal Site of Nick Girardo

This website is built with [Hakyll](https://jaspervdj.be/hakyll/) and makes heavy use of [Nix](https://nixos.org/).

## Structure

Many of the assets required for fully building this site are external to this repository. For instance, my resume which is listed at `/resume.pdf` is described in [this repository](https://github.com/nickgirardo/resume). Build instructions for all of these external dependencies are defined by Nix (e.g. [my resume's flake.nix](https://github.com/nickgirardo/resume/blob/master/flake.nix)) and are listed as inputs in [this repository's flake.nix](https://github.com/nickgirardo/hakyll-site/blob/master/flake.nix).

This means that building the website in full requires Nix (or manually replicating the work Nix would handle). While you can build the site without Nix (i.e. using Stack or Cabal) you will lack the externally defined assets and only have a small shell of the full site.

As these external dependencies are identified by their git repo and commit hash, updating them is as simple as updating their commit hashes in `flake.nix`.

In the prebuild phase, copy all external dependencies to the `extern/` directory. All files in `extern/` have their prefix removed but are otherwise unchanged by Hakyll in the build phase. For instance, in the prebuild phase we move `resume.pdf` from the `resume` input into `extern/resume.pdf`. The build phase copies this over to `/resume.pdf`.

## Building

This site can be built with `nix build`.

## Running

This site can be ran manually with `nix run`, which automatically serves the built site on port 8080. An alternative port can be specified as an argument; to serve on port 9000 run `nix run . -- 9000`.
