{}:

let
  pkgs = import ./nix {};

  drv =
    pkgs.haskell.lib.addBuildDepends
      (pkgs.haskellPackages.callCabal2nix "hpc-coveralls" ./hpc-coveralls.cabal {})
      (with pkgs.haskellPackages;
        [ cabal2nix
          cabal-install
        ] ++ (with pkgs; [ git gitAndTools.gitflow jq ghcid ])
      );

in
  if pkgs.lib.inNixShell then drv.env else drv
