{ naersk, pkg-config }:

naersk.buildPackage {
  name = "arcana";
  src = ./.;
  nativeBuildInputs = [ pkg-config ];
  doCheck = false;

  cargoBuildFlags = [ "--bin" "mage" ];
}
