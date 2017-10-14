{ mkDerivation, base, gloss, stdenv }:
mkDerivation {
  pname = "tubes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base gloss ];
  executableHaskellDepends = [ base gloss ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/tubes#readme";
  license = stdenv.lib.licenses.bsd3;
}
