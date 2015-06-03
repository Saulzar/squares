{ mkDerivation, 
    squares,
    reflex, 
    reflex-dom, 
    file-embed, 
    ghcjs-websockets,
    ghcjs-websockets-reflex,
    containers,
    text,
    transformers,
    lens,
    linear,
    dependent-sum,
    hashable,
    binary,
    time
}:

mkDerivation {
  pname = "squares-client";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    squares
    reflex
    reflex-dom
    ghcjs-websockets
    ghcjs-websockets-reflex
    containers
    text
    transformers
    file-embed
    lens
    linear
    dependent-sum
    hashable
    binary
    time
    
  ];
  license = null;
}
