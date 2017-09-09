self: super: with super.lib; (foldl' (flip extends) (_: super) [
  (import ./dapphub-overlay.nix)
]) self
