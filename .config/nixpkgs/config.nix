{
  allowUnfreePredicate = with builtins;
  let
    getName = pkg: (parseDrvName pkg.name).name;
  in pkg: elem (getName pkg) [
    "slack"
    "spotify"
  ];
  pulseaudio = true;
}
