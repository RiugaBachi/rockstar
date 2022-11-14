# rockstar

A modern blog theme written in pure Haskell using the [rib](https://github.com/srid/rib) static site generator. Loosely based off of the Taylantatli/Moon Jekyll theme.

For a live sample, this theme powers my blog at https://riugabachi.github.io

## Prerequisites

Clone this repository and edit the global site configuration at the top of `./app/Main.hs` as appropriate. They are, by default, configured for my blog.

## Workflow

To build and run the site:

```bash
nix-shell --run 'ghcid -T ":main -wS"'
```

This requires the [ghcid](https://github.com/ndmitchell/ghcid) wrapper tool.

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `./app/Main.hs`, `./app/Style.hs`, or the content in `./content` 
reloads everything.

For Github / Gitlab pages, you can init a git repository inside the `site` output folder and push to your page repo manually.
