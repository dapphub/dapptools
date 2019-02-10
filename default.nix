{
  system ? null,
  android ? false,
  ...
}:
let
  version = "6a0d2ff7c1d024914a3570b85f1c88df8930b471";
  # Import a specific Nixpkgs revision to use as the base for our overlay.
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-${version}";
    url = "https://github.com/nixos/nixpkgs/archive/${version}.tar.gz";
    sha256 = "06q10786lj7yig6spzsz1zflcjqxjj11d5qsr305jln34wndlj27";
  };
in
  # Now return the Nixpkgs configured to use our overlay.
  import nixpkgs (
    {
      overlays = [
        (import ./overlay.nix)
      ] ++ (if android then [(import ./overlay-android.nix)] else []);
    } // (
      if system != null then { inherit system; } else {}
    )
  )
