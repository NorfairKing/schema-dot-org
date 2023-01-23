{ mkDerivation, aeson, base, lib, scientific, text }:
mkDerivation {
  pname = "schema-dot-org";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base scientific text ];
  homepage = "https://github.com/NorfairKing/schema-dot-org#readme";
  license = "unknown";
}
