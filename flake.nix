{
  description = "flake-pin";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  };

  outputs =
    { self, nixpkgs }:
    let
      ghc = "ghc984"; # 9.8.4

      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f { pkgs = (pkgsFor system); });

      pkgsFor = system: import nixpkgs { inherit system; };

    in
    {

      devShells = forAllSystems (
        { pkgs }:
        {
          default = pkgs.mkShell {
            buildInputs = [ pkgs.zlib ];
            packages = [
              pkgs.cabal-install
              pkgs.haskell.compiler.${ghc}
              pkgs.haskell.packages.${ghc}.cabal-gild
              pkgs.haskell.packages.${ghc}.haskell-language-server
              pkgs.hlint
              pkgs.fourmolu
            ];
          };
        }
      );
    };
}
