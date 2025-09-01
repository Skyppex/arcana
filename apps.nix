{
  arcanaPackage,
  pkgs,
}: rec {
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
}
