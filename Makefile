all :
	nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ doctest QuickCheck text parsec ])"
