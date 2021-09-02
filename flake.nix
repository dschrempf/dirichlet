{
  description = "Multivariate Dirichlet distribution";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            haskellPackages = pkgs.haskellPackages;
            dirichlet = haskellPackages.callCabal2nix "dirichlet" self rec {};
            dirichlet-dev = pkgs.haskell.lib.doBenchmark dirichlet;
          in
            {
              packages.circular = dirichlet;

              defaultPackage = dirichlet;

              devShell = pkgs.haskellPackages.shellFor {
                packages = _: [ dirichlet-dev ];
                buildInputs = with pkgs; [
                  bashInteractive
                  haskellPackages.cabal-install
                  haskellPackages.haskell-language-server
                  haskellPackages.stack
                ];
                doBenchmark = true;
                withHoogle = true;
              };
            }
      );
}
