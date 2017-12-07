{ config, pkgs, ... }: {
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
  ];

  environment.systemPackages = with pkgs; [
    seth
    ethsign
    qrencode
  ];

  services.xserver.enable = true;
  services.xserver.displayManager.slim = {
    enable = true;
    defaultUser = "root";
    autoLogin = true;
  };

  services.xserver.desktopManager = {
    default = "ethos";
    session = [{
      name = "ethos";
      start = ''
        ${pkgs.ratpoison}/bin/ratpoison &
        ${pkgs.xterm}/bin/xterm &
      '';
    }];
  };
}
