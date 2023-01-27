{ mkDerivation, aeson, base, lib, schema-dot-org, text }:
mkDerivation {
  pname = "schema-dot-org-jsonld";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base schema-dot-org text ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
}
