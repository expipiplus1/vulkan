{ pkgs ? import ./nixpkgs.nix, compiler ? null
, vulkan-docs ? ../generate-new/Vulkan-Docs, withDocs ? false }:

with pkgs;
with lib;

let haskellPackages = import ./haskell-packages.nix { inherit pkgs compiler; };

in runCommand "vulkan-gen" {
  nativeBuildInputs = [
    python3
    python3Packages.pyparsing
    asciidoctor
    gnumake
    nodejs
    nodePackages.he
    nodePackages.escape-string-regexp
    git
  ];
} ''
  cp -r "${vulkan-docs}" Vulkan-Docs
  ${optionalString withDocs ''
    chmod a+w Vulkan-Docs --recursive
    patchShebangs Vulkan-Docs/makeAllExts Vulkan-Docs/makeSpec

    echo "Cleaning Vulkan-Docs"
    git -C Vulkan-Docs clean -dxf || true

    echo "Generating Vulkan-Docs documentation"
    (cd Vulkan-Docs && ./makeAllExts refpages generated)
  ''}
  LANG=C.UTF-8 ${haskellPackages.generate-new}/bin/vk ${
    optionalString (!withDocs) "nodocs"
  }
  mv out "$out"
''
