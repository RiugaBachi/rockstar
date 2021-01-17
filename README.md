# festive

A modern blog theme written in pure Haskell using the [rib](https://github.com/srid/rib) static site generator. Loosely based off of the Taylantatli/Moon Jekyll theme.

For a live sample, this theme powers my blog at https://riugabachi.github.io

## Prerequisites

Clone this repository and edit the global site configuration at the top of `./app/Main.hs` as appropriate. They are, by default, configured for my blog.

Note that `rib` currently does not build on GHC 8.10.x; 8.8.x works, however. Until it builds on 8.10.x, the constraint on `base` shall be kept at `< 4.14` to mark this lack of support.

## Workflow

To build and run the site:

```bash
ghcid -T ":main -wS"
```

This requires the [ghcid](https://github.com/ndmitchell/ghcid) wrapper tool. Alternatively, you can `cabal build && cabal run site -- -wS` or `nix-build` manually.

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `./app/Main.hs`, `./app/Style.hs`, or the content in `./content` 
reloads everything.

A `default.nix` file is included for your covenience should you prefer using nix to install and run ghcid. For more information, please consult the `rib` repository.

For Github / Gitlab pages, you can init a git repository inside the `site` output folder and push to your page repo manually.
