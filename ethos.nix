{ config, pkgs, ... }: {
  imports = [
    ./nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix
  ];

  networking.hostName = "ethos";
  i18n.consoleFont = "sun12x22";

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
        ${pkgs.ratpoison}/bin/ratpoison -f /etc/ratpoisonrc &
        ${pkgs.xterm}/bin/xterm -r -fa Iosevka -fs 16 &
        wait
      '';
    }];
  };

  environment.etc."ratpoisonrc".text = ''
set startupmessage off
set bargravity c
set padding 10 10 10 10
set border 6
set fwcolor black
set barpadding 8 4
exec xsetroot -solid indigo
echo Welcome to Ethos.
  '';

  environment.etc."bashrc.local".text = ''
HISTCONTROL=erasedups
HISTSIZE=99999
[[ $PS1 ]] || return
PS1=$'\[\e[1m\]\h\[\e[0m\]:\$ '
cat /etc/ethos-help
  '';

  environment.etc."ethos-help".text = ''

            ████████ ██████████ ██      ██   ███████    ████████
           ░██░░░░░ ░░░░░██░░░ ░██     ░██  ██░░░░░██  ██░░░░░░
           ░██          ░██    ░██     ░██ ██     ░░██░██
           ░███████     ░██    ░██████████░██      ░██░█████████
           ░██░░░░      ░██    ░██░░░░░░██░██      ░██░░░░░░░░██
           ░██          ░██    ░██     ░██░░██     ██        ░██
           ░████████    ░██    ░██     ░██ ░░███████   ████████
           ░░░░░░░░     ░░     ░░      ░░   ░░░░░░░   ░░░░░░░░

                 Ethereum distribution (NixOS) by DappHub.

  '';
}
