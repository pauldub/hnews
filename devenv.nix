{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [];

  # https://devenv.sh/languages/
  languages.nix.enable = true;
  languages.haskell.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
