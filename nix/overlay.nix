final: prev:
with final.lib;
with final.haskell.lib;
{
  schema-dot-org-generator = justStaticExecutables final.haskellPackages.schema-dot-org-generator;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          {
            schema-dot-org-generator = buildStrictly (self.callPackage ../schema-dot-org-generator { });
          }
      );
  });
}
