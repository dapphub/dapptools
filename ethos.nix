{ config, pkgs, ... }: {
  imports = [
    ./nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix
  ];

  environment.systemPackages = with pkgs; [
    seth
    ethsign
    qrencode
    feh
  ];

  users.extraUsers.ethos = {
    isNormalUser = true;
    uid = 1000;
  };

  services.xserver.enable = true;
  services.xserver.libinput.enable = true;
  services.xserver.xkbOptions = "ctrl:nocaps";
  services.xserver.displayManager.slim = {
    enable = true;
    defaultUser = "ethos";
    autoLogin = true;
  };

  services.xserver.desktopManager = {
    default = "ethos";
    session = [{
      name = "ethos";
      start = ''
        ${pkgs.xorg.xsetroot}/bin/xsetroot -solid black
        ${pkgs.ratpoison}/bin/ratpoison &
        ${pkgs.xterm}/bin/xterm -r -fn 10x20 &
        wait
      '';
    }];
  };

  environment.etc."bashrc.local".text = ''
HISTCONTROL=erasedups
HISTSIZE=99999
PS1=$'\[\e[1m\]\h\[\e[0m\]:\$ '
  '';
}
