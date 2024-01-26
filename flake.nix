{
  description = "lyahfgg";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs: let
    overlay = final: prev: {
      haskell =
        prev.haskell
        // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev
            // {
              lyahfgg = hfinal.callCabal2nix "lyahfgg" ./. {};
            };
        };
      lyahfgg = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.lyahfgg;
    };
    perSystem = system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [overlay];
      };
      hspkgs = pkgs.haskellPackages;
    in {
      devShell = hspkgs.shellFor {
        withHoogle = true;
        packages = p: [p.lyahfgg];
        buildInputs = [
          hspkgs.cabal-install
          hspkgs.ghcid
          hspkgs.ormolu
          hspkgs.hlint
          hspkgs.haskell-language-server
        ];
      };
      defaultPackage = pkgs.lyahfgg;
    };
  in
    {inherit overlay;} // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
