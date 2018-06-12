{ flavor }: self: super:
import ./overlay.nix { inherit self super flavor; }
