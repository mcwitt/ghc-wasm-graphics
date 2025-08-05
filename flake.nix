{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  inputs.ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
  inputs.git-hooks.url = "github:cachix/git-hooks.nix";

  outputs =
    {
      self,
      nixpkgs,
      ghc-wasm-meta,
      git-hooks,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      forAllSystems' = nixpkgs.lib.genAttrs systems;
      mkPkgs = system: nixpkgs.legacyPackages.${system};
      forAllSystems = f: forAllSystems' (system: f (mkPkgs system) system);
    in
    {
      checks = forAllSystems' (system: {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            cabal-fmt.enable = true;
            hlint.enable = true;
            ormolu.enable = true;

            html-tidy.enable = true;

            nixfmt-rfc-style.enable = true;
          };
        };
      });

      devShells = forAllSystems (
        pkgs: system:
        let
          haskellPackages = pkgs.haskell.packages.ghc912;
        in
        {
          default = pkgs.mkShellNoCC {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
            packages = [
              ghc-wasm-meta.packages."${system}".all_9_12

              pkgs.wabt

              haskellPackages.ghc
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server

              pkgs.python3 # for dev http server
            ] ++ self.checks.${system}.pre-commit-check.enabledPackages;
          };
        }
      );
    };
}
