{ mkDerivation, containers,
    text,
    transformers,
    file-embed,
    lens,
    linear,
    binary,
    text-binary
}:

mkDerivation {
  pname = "squares";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = false;
  buildDepends = [
    containers
    text
    transformers
    file-embed
    lens
    linear
    binary
    text-binary
  ];
  license = null;
}
