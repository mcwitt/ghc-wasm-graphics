{
  inputs.ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

  outputs =
    {
      self,
      ghc-wasm-meta,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      inherit (ghc-wasm-meta.inputs) nixpkgs;

      forAllSystems' = nixpkgs.lib.genAttrs systems;
      mkPkgs = system: nixpkgs.legacyPackages.${system};
      forAllSystems = f: forAllSystems' (system: f (mkPkgs system) system);
    in
    {
      devShells = forAllSystems (
        pkgs: system:
        let
          haskellPackages = pkgs.haskell.packages.ghc912;
        in
        {
          default = pkgs.mkShellNoCC {
            packages = [
              ghc-wasm-meta.packages."${system}".all_9_12

              pkgs.wabt

              haskellPackages.ghc
              haskellPackages.cabal-install
              haskellPackages.ormolu
              haskellPackages.haskell-language-server

              pkgs.python3 # for dev http server
            ];
          };
        }
      );
    };
}
