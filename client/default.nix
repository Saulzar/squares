{ mkDerivation, 
    reflex, 
    reflex-dom, 
    file-embed, 
    ghcjs-websockets,
    containers,
    text,
    transformers,
    lens,
    linear,
    dependent-sum
}:

mkDerivation {
  pname = "squares-client";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    reflex
    reflex-dom
    ghcjs-websockets
    containers
    text
    transformers
    file-embed
    lens
    linear
    dependent-sum
    
  ];
  license = null;
}
