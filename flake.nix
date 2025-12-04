{
  description = "schema-dot-org";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.haskellPackages.schema-dot-org;
        jsonld = pkgs.haskellPackages.schema-dot-org-jsonld;
        generator = pkgs.haskellPackages.schema-dot-org-generator;
      };
      checks.${system} = {
        jsonld = self.packages.${system}.jsonld;
        release = self.packages.${system}.default;
        generator = self.packages.${system}.generator;
        shell = self.devShells.${system}.default;
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "schema-dot-org-shell";
        packages = p: [ p.schema-dot-org-generator ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs =
          (with pkgs; [
            cabal-install
            graphviz
            xdot
            niv
            zlib
          ]) ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
