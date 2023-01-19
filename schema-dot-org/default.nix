{ mkDerivation, aeson, base, lib, text }:
mkDerivation {
  pname = "schema-dot-org";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
}
