{ mkDerivation,     
    websockets,
    text,
    containers,
    transformers,
    bytestring,
    stm,
    lens,
    lifted-base,
    linear,
    squares,
    binary,
    aeson
}:

mkDerivation {
  pname = "squares-server";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    websockets
    text
    containers
    transformers
    bytestring
    stm
    lens
    lifted-base
    linear
    squares
    binary
    aeson
  ];
  license = null;
}
