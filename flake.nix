{
  description = "schema-dot-org";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-26.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    # sydtest comes from git rather than from nixpkgs: the test suite uses the
    # typed scenario paths, which nixpkgs' sydtest does not have yet.
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , autodocodec
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (validity + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
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
