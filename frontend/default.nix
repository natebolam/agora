{ pkgs, src ? ./. }:

pkgs.buildNpmPackage {
  inherit src;

  SASS_BINARY_PATH = pkgs.fetchurl {
    url = https://github.com/sass/node-sass/releases/download/v4.12.0/linux-x64-64_binding.node;
    sha256 = "0dl91l414na44h090cgghd06q0j2whlj9h98im2qb9823glq7xff";
  };

  buildInputs = with pkgs; [
    utillinux # lscpu for parcel
  ];

  doCheck = true;
  checkPhase = ''
    npm run tslint
    npm run stylelint
    npm run tscompile
    npm run test
  '';

  npmBuildMore = ''
    npm run build
  '';

  installPhase = ''
    mv dist $out
  '';
}
