final: prev:
with final.lib;
with final.haskell.lib;
{
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super: {
          schema-dot-org = buildStrictly (self.callPackage ../schema-dot-org { });
          schema-dot-org-jsonld = buildStrictly (self.callPackage ../schema-dot-org-jsonld { });
          schema-dot-org-generator = buildStrictly (self.callPackage ../schema-dot-org-generator { });
        }
      );
  });
}
