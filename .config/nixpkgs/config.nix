{
  allowUnfreePredicate =
  let nixpkgs = import <nixpkgs> {};
  in pkg: builtins.elem (nixpkgs.lib.getName pkg) [
    "slack"
    "spotify"
    "spotify-unwrapped"
  ];
  pulseaudio = true;
}
