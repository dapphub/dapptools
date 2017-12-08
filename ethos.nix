{ config, pkgs, ... }: {
  imports = [
    ./nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix
  ];

  networking.hostName = "ethos";

  users.extraUsers.ethos = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = ["usb"];
  };

  i18n.consoleFont = "sun12x22";

  environment.systemPackages = with pkgs; [
    ethsign
    seth
    minimodem
    ncurses

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
  services.udev.extraRules = let
    usbHook = pkgs.bashScript {
      name = "usb-hook";
      deps = [pkgs.ratpoison pkgs.coreutils];
      text = ''
        if [ -v ID_VENDOR_ID ]; then
          text="echo USB $ACTION $ID_VENDOR_ID:$ID_MODEL_ID"
          DISPLAY=:0 sudo -u ethos ratpoison -c "$text"
        fi
      '';
    };
  in ''
    SUBSYSTEM=="usb",    ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0001", MODE="0660", GROUP="usb"
    SUBSYSTEM=="hidraw", ATTRS{idVendor}=="2c97", KERNEL=="hidraw*",        MODE="0660", GROUP="usb"
    SUBSYSTEM=="usb",    RUN+="${usbHook}/bin/usb-hook"
  '';

  fonts.fontconfig.hinting.enable = false;
  fonts.fontconfig.subpixel.rgba = "none";
  fonts.fonts = [pkgs.iosevka-term];

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
        ethos-terminal &
        wait
      '';
    }];
  };

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
spaces=$(head -c 16 < /dev/zero | tr '\0' ' ')
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
