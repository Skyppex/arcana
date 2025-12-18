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
            "rust-docs"
            "rust-std"
            "rust-analyzer"
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
        inherit pkgs;
      };

      apps = import ./apps.nix {
        inherit arcanaPackage;
        inherit pkgs;
      };
    in {
      packages = rec {
        default = debug;
        debug = arcanaPackage {release = false;};
        release = arcanaPackage {release = true;};
      };

      inherit checks;
      inherit apps;

      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          toolchain
          nil
          alejandra
        ];

        env.RUST_SRC_PATH = "${toolchain}/lib/rustlib/src/rust/library";
      };

      formatter = pkgs.writeShellApplication {
        name = "fmt";
        runtimeInputs = [pkgs.rustfmt pkgs.cargo];
        text = "cargo fmt --all";
      };
    });
}
