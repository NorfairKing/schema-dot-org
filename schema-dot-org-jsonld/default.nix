{ mkDerivation, aeson, base, bytestring, lib, schema-dot-org
, sydtest, sydtest-aeson, sydtest-discover, tagsoup, text, vector
}:
mkDerivation {
  pname = "schema-dot-org-jsonld";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring schema-dot-org tagsoup text vector
  ];
  testHaskellDepends = [
    aeson base bytestring sydtest sydtest-aeson
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
}
