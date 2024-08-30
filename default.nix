with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "build-environment";
  buildInputs = [ zlib ];
}
