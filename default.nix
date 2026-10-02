{
  src,
  naersk,
  pkgs,
  pkgConfig,
  release ? false,
}:
naersk.buildPackage {
  name = "arcana";
  inherit src;
  nativeBuildInputs = [pkgConfig pkgs.makeWrapper];
  doCheck = false;

  cargoBuildFlags =
    ["--bin" "mage"]
    ++ (
      if release
      then ["--release"]
      else []
    );

  # The core library is an ordinary spell, read from disk at runtime rather than
  # compiled in, so it has to be installed alongside the binary.
  #
  # `--set-default` rather than `--set`: the path compiled into the binary points
  # into the build sandbox and is useless here, but someone pointing mage at a
  # checkout of core should still be able to.
  postInstall = ''
    mkdir -p $out/share/arcana
    cp -r ${src}/core $out/share/arcana/core

    wrapProgram $out/bin/mage \
      --set-default ARCANA_CORE_LIB_PATH $out/share/arcana/core
  '';
}
