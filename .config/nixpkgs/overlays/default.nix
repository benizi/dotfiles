self: { stdenv, ... }@super:
{
  hello = stdenv.mkDerivation rec {
    name = "hello";
    src = super.fetchurl {
      url = "mirror://gnu/hello/${name}.tar.gz";
      sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
    };
  };
}
