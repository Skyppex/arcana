{
  description = "arcana flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    naersk,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      # system = "x86_64-linux";
      pkgs = import nixpkgs {inherit system;};
      naerskLib = pkgs.callPackage naersk {};

      arcanaPackage = {release}:
        import ./default.nix {
          naersk = naerskLib;
          pkg-config = pkgs.pkg-config;
          inherit release;
        };
    in {
      packages = rec {
        default = debug;
        debug = arcanaPackage {release = false;};
        release = arcanaPackage {release = true;};
      };

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [cargo rustc rustfmt clippy rust-analyzer];
        env.RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
      };

      checks = {
        cargo-check = naerskLib.buildPackage {
          src = self;
          mode = "check";
        };

        cargo-test = naerskLib.buildPackage {
          src = self;
          mode = "test";
        };

        cargo-clippy = naerskLib.buildPackage {
          src = self;
          mode = "clippy";
        };

        cargo-fmt =
          pkgs.runCommand "cargo-fmt-check" {
            buildInputs = [pkgs.rustfmt pkgs.cargo];
          } ''
            cp -r ${self} ./source
            chmod -R +w ./source
            cd ./source
            cargo fmt --all -- --check
            touch $out
          '';
      };

      apps = rec {
        default = mage;

        mage = {
          type = "app";
          program = "${arcanaPackage {release = false;}}/bin/mage";
        };

        fix = {
          type = "app";
          program = "${pkgs.writeShellApplication {
            name = "cargo-fix";
            runtimeInputs = [pkgs.cargo pkgs.clippy];
            text = ''
              cargo clippy --fix --allow-dirty --allow-staged --all-targets
            '';
          }}/bin/cargo-fix";
        };
      };

      formatter = pkgs.writeShellApplication {
        name = "fmt";
        runtimeInputs = [pkgs.rustfmt pkgs.cargo];
        text = "cargo fmt --all";
      };
    });
}
