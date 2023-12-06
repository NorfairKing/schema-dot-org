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
            ghc-source-gen = self.callCabal2nix "ghc-source-gen"
              (builtins.fetchGit {
                url = "https://github.com/google/ghc-source-gen";
                rev = "7527305ed59a47140053cf7bc87432fe1f8804d0";
              })
              { };
            schema-dot-org = buildStrictly (self.callPackage ../schema-dot-org { });
            schema-dot-org-generator = buildStrictly (self.callPackage ../schema-dot-org-generator { });
            schema-dot-org-jsonld = buildStrictly (self.callPackage ../schema-dot-org-jsonld { });
          }
      );
  });
}
