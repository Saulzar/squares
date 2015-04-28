{ mkDerivation,     
    websockets,
    text,
    containers,
    transformers,
    aeson,
    generic-aeson,
    bytestring,
    stm,
    lens,
    lifted-base,
    linear
}:

mkDerivation {
  pname = "reflex-todomvc";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    websockets
    text
    containers
    transformers
    aeson
    generic-aeson
    bytestring
    stm
    lens
    lifted-base
    linear
  ];
  license = null;
}
