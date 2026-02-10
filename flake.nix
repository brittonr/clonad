{
  description = "Clonad - A Claude Foreign Function Interface for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            clonad = final.callCabal2nix "clonad" ./. { };
          };
        };

        clonad = haskellPackages.clonad;
      in
      {
        packages = {
          default = clonad;
          clonad = clonad;
        };

        apps.default = {
          type = "app";
          program = "${clonad}/bin/clonad-server";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.clonad ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ormolu
            ghcid
          ];
          withHoogle = true;
        };
      });
}
