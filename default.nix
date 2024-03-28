let
  ribSrc =  ../rib/.;
in {
# Rib library source to use
  rib ? ribSrc
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "rockstar"
, ...
}:

let 
  # Add your Haskell package overrides here
  source-overrides = {
  };
in import rib { 
  inherit root name source-overrides; 
}
