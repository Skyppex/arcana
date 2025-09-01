{
  src,
  naersk,
  pkgConfig,
  release ? false,
}:
naersk.buildPackage {
  name = "arcana";
  inherit src;
  nativeBuildInputs = [pkgConfig];
  doCheck = false;

  cargoBuildFlags =
    ["--bin" "mage"]
    ++ (
      if release
      then ["--release"]
      else []
    );
}
