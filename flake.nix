{
  description = "Multivariate Dirichlet distribution";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc924;
        dirichlet = hpkgs.callCabal2nix "dirichlet" self rec { };
        dirichlet-dev = pkgs.haskell.lib.doBenchmark dirichlet;
      in
      {
        packages.default = dirichlet;

        devShells.default = hpkgs.shellFor {
          packages = _: [ dirichlet-dev ];
          buildInputs = with pkgs; [
            bashInteractive

            hpkgs.cabal-fmt
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
