{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, containers, envparse, ghc, ghc-paths, ghc-source-gen
, graphviz, lib, optparse-applicative, path, path-io, pretty, text
, typed-process, yaml
}:
mkDerivation {
  pname = "schema-dot-org-generator";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring containers
    envparse ghc ghc-paths ghc-source-gen graphviz optparse-applicative
    path path-io pretty text typed-process yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
  mainProgram = "schema-dot-org-generator";
}
