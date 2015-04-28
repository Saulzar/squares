{ mkDerivation
}:

mkDerivation {
  pname = "squares";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [

  ];
  license = null;
}
