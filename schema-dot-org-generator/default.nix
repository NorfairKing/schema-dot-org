{ mkDerivation, aeson, autodocodec, base, bytestring, containers
, ghc, ghc-paths, ghc-source-gen, lib, process, text
}:
mkDerivation {
  pname = "schema-dot-org-generator";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring containers ghc ghc-paths
    ghc-source-gen process text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
  mainProgram = "schema-dot-org-generator";
}
