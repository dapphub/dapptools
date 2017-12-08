{ config, pkgs, ... }: {
  imports = [
    ./nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix
  ];

  # Set the hostname, mostly just for the bash prompt.
  networking.hostName = "ethos";

  # Make an `ethos' user with access to the USB group.
  users.extraUsers.ethos = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = ["usb"];
  };

  # Use a bigger TTY font.
  i18n.consoleFont = "sun12x22";

  # Make relevant programs available in the system $PATH.
  environment.systemPackages = with pkgs; [
    ethsign
    seth
    minimodem
    ncurses

    # A script for showing QR codes from hex e.g. "0xabcd"
    (bashScript {
      name = "qr";
      deps = [qrencode feh vim gnused coreutils];
      text = ''
        sed 's/^0x//' | tr -d '[:space:]' | xxd -r -p | base64 -w0 | \
          qrencode -s 1 -o - | feh -ZB white --force-aliasing -
      '';
    })
  ];

  # Enable Ledger Nano S support.
  services.udev.extraRules = ''
    SUBSYSTEM=="usb",    ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001", MODE="0660", GROUP="usb"
    SUBSYSTEM=="hidraw", ATTRS{idVendor}=="2c97", KERNEL=="hidraw*",        MODE="0660", GROUP="usb"
  '';

  # Font configuration
  fonts.fontconfig.hinting.enable = false;
  fonts.fontconfig.subpixel.rgba = "none";
  fonts.fonts = [pkgs.iosevka-term];

  # Set up the X server
  services.xserver.enable = true;
  services.xserver.libinput.enable = true;
  services.xserver.xkbOptions = "ctrl:nocaps";
  services.xserver.displayManager.slim = {
    enable = true;
    defaultUser = "ethos";
    autoLogin = true;
  };

  # Start Ratpoison as the default window manager
  services.xserver.desktopManager = {
    default = "ethos";
    session = [{
      name = "ethos";
      start = ''
        ${pkgs.ratpoison}/bin/ratpoison -f /etc/ratpoisonrc &
        ethos-terminal &
        wait
      '';
    }];
  };

  # Ratpoison configuration
  environment.etc."ratpoisonrc".text = ''
    set font "Iosevka Term-16"
    set startupmessage off
    set bargravity c
    set padding 10 10 10 10
    set border 6
    set fwcolor black
    set barpadding 8 4
    bind d exec setxkbmap dvorak
    exec xsetroot -solid indigo
    echo Welcome to Ethos.
    bind c exec xterm -fa "Iosevka Term" -fs 16
    exec xterm -fa "Iosevka Term" -fs 16
  '';

  environment.etc."bashrc.local".text = ''
HISTCONTROL=erasedups
HISTSIZE=99999
[[ $PS1 ]] || return
PS1=$'\[\e[1m\]\h\[\e[0m\]:\$ '
x=$(tput width)
x=$((w - 53) / 2)
spaces=$(head -c "$x" < /dev/zero | tr '\0' ' ')
cat /etc/ethos-help | sed "s/^/$spaces/"
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
