{
  naersk,
  pkg-config,
  release ? false,
}:
naersk.buildPackage {
  name = "arcana";
  src = ./.;
  nativeBuildInputs = [pkg-config];
  doCheck = false;

  cargoBuildFlags =
    ["--bin" "mage"]
    ++ (
      if release
      then ["--release"]
      else []
    );
}
