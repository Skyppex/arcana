{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  # https://devenv.sh/packages/
  packages = with pkgs; [
    alejandra
  ];

  # https://devenv.sh/languages/
  languages.rust.enable = true;
  languages.nix.enable = true;
}
