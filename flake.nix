{
  description = "arcana flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
  };

  outputs = { self, nixpkgs, flake-utils, naersk, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # system = "x86_64-linux";
        pkgs = import nixpkgs { inherit system; };
        naerskLib = pkgs.callPackage naersk { };

        arcanaPackage = import ./default.nix {
          naersk = naerskLib;
          pkg-config = pkgs.pkg-config;
        };
      in {
        # packages.default = arcanaPackage;
        packages.default = naerskLib.buildPackage {
          src = self;
          nativeBuildInputs = [ pkgs.pkg-config ];
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [ cargo rustc rustfmt clippy rust-analyzer ];
          env.RUST_SRC_PATH =
            "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
        };

        apps = rec {
          mage = {
            type = "app";
            program = "${arcanaPackage}/bin/mage";
          };
          default = mage;
        };
      });
}
