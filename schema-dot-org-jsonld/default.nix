{ mkDerivation, aeson, base, bytestring, lib, scalpel
, schema-dot-org, tagsoup, text, vector
}:
mkDerivation {
  pname = "schema-dot-org-jsonld";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring scalpel schema-dot-org tagsoup text vector
  ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
}
