{
  description = "arcana flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    naersk,
    fenix,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      # system = "x86_64-linux";
      pkgs = import nixpkgs {inherit system;};
      fenixLib = fenix.packages.${system};
      toolchain = with fenixLib;
        combine [
          (stable.withComponents [
            "rustc"
            "cargo"
            "rustfmt"
            "clippy"
            "rust-src"
          ])
        ];

      naerskLib = (pkgs.callPackage naersk {}).override {
        cargo = toolchain;
        rustc = toolchain;
      };

      arcanaPackage = {release}:
        import ./default.nix {
          src = self;
          naersk = naerskLib;
          pkgConfig = pkgs.pkg-config;
          inherit release;
        };
      checks = import ./checks.nix {
        src = self;
        naersk = naerskLib;
        pkgs = pkgs;
      };
      apps = import ./apps.nix {
        arcanaPackage = arcanaPackage;
        pkgs = pkgs;
      };
    in {
      packages = rec {
        default = debug;
        debug = arcanaPackage {release = false;};
        release = arcanaPackage {release = true;};
      };

      devShells.default = pkgs.mkShell {
        packages = [toolchain];
        env.RUST_SRC_PATH = "${toolchain}/lib/rustlib/src/rust/library";
      };

      checks = checks;

      apps = apps;

      formatter = pkgs.writeShellApplication {
        name = "fmt";
        runtimeInputs = [pkgs.rustfmt pkgs.cargo];
        text = "cargo fmt --all";
      };
    });
}
