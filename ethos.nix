{ hidpi ? false } :

{ config, pkgs, ... }: let

  usb = { ledger.vendor = "2c97"; };

  # usb = { ledger.vendor = "05ac"; }; # iPhone ID, for testing w/o Ledger

in {
  imports = [
    ./ethos-iso-image.nix
    ./nixpkgs/nixos/modules/profiles/all-hardware.nix
  ];

  fonts.fontconfig.dpi = if hidpi then 200 else 96;

  isoImage.isoName = if hidpi then "ethos-hidpi.iso" else "ethos.iso";
  isoImage.volumeID = pkgs.lib.substring 0 11 "NIXOS_ISO";
  isoImage.makeEfiBootable = true;
  isoImage.makeUsbBootable = true;

  networking.hostName = "ethos";

  users.extraUsers.ethos = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = ["usb"];
  };

  i18n.consoleFont = "sun12x22";

  environment.systemPackages = with pkgs; [
    ethsign seth bc ethabi
    xorg.xsetroot

    (bashScript {
      name = "qr";
      deps = [qrencode feh vim gnused coreutils];
      text = ''
        sed 's/^0x//' | tr -d '[:space:]' | xxd -r -p | base64 -w0 | \
          qrencode -s 1 -o - | feh -ZB white --force-aliasing -
      '';
    })

    (bashScript {
      name = "battery";
      deps = [upower gnugrep gnused];
      check = false;
      text = ''
        exec < <(upower -i `upower -e | grep BAT` | grep :. | sed 's/^ *//')
        declare -A BAT; while IFS=: read k v; do BAT[$k]=`echo $v`; done
        time=(''${BAT['time to full']}''${BAT['time to empty']})
        time=''${time[0]}''${time[1]:0:1}
        rate=(''${BAT[energy-rate]}) rate=''${rate[0]%.*}''${rate[1]}
        percent=''${BAT[percentage]}
        state=''${BAT[state]}
        echo $percent $state $rate $time
      '';
    })
  ];

  security.sudo.enable = true;
  services.upower.enable = true;

  # Enable Ledger Nano S support.
  services.udev.extraRules = let
    usbHook = pkgs.bashScript {
      name = "usb-hook";
      deps = [pkgs.ratpoison pkgs.coreutils pkgs.xorg.xsetroot];
      text = ''
        export DISPLAY=:0
        if [ -v ID_VENDOR_ID ]; then
          if [ "$ID_VENDOR_ID" = ${usb.ledger.vendor} ]; then
            if [ "$ACTION" = add ]; then
              text="Ledger device connected"
              sudo -u ethos xsetroot -solid gold
            else
              text="Ledger device unplugged"
              sudo -u ethos xsetroot -solid indigo
            fi
            sudo -u ethos ratpoison -c "echo $text"
          fi
        fi
      '';
    };
  in ''
    SUBSYSTEM=="usb",    ATTRS{idVendor}=="${usb.ledger.vendor}", ATTRS{idProduct}=="0001", MODE="0660", GROUP="usb"
    SUBSYSTEM=="hidraw", ATTRS{idVendor}=="${usb.ledger.vendor}", KERNEL=="hidraw*",        MODE="0660", GROUP="usb"
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
        if ${pkgs.usbutils}/bin/lsusb -d ${usb.ledger.vendor}: ; then
          xsetroot -solid gold
          echo
        fi
        ${pkgs.ratpoison}/bin/ratpoison -f /etc/ratpoisonrc &
        ${pkgs.sxhkd}/bin/sxhkd -c /etc/sxhkdrc &
        wait
      '';
    }];
  };

  environment.etc.sxhkdrc.text = ''
    XF86MonBrightnessUp
      ${pkgs.xlibs.xbacklight}/bin/xbacklight +20
    XF86MonBrightnessDown
      ${pkgs.xlibs.xbacklight}/bin/xbacklight =1
    Print
      ${pkgs.ratpoison}/bin/ratpoison -c "echo `battery`"
  '';

  environment.etc."ratpoisonrc".text = ''
    set font "Iosevka Term-16"
    set startupmessage 0
    set bargravity c
    set wingravity center
    set padding 30 30 30 30
    set border 6
    set fwcolor black
    set barpadding 8 4
    bind d exec setxkbmap dvorak
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

    if ${pkgs.usbutils}/bin/lsusb -d ${usb.ledger.vendor}: ; then
      echo
    fi

    set -u # fail on missing variables
  '';

  environment.variables = {
    DAI_MULTISIG_ADDRESS = "0x7Bb0b08587b8a6B8945e09F1Baca426558B0f06a";
    MKR_REDEEMER_ADDRESS = "0x642AE78FAfBB8032Da552D619aD43F1D81E4DD7C";
    MKR_TOKEN_ADDRESS = "0x9f8F72aA9304c8B593d555F12eF6589cC3A579A2";
    OLD_MKR_TOKEN_ADDRESS = "0xC66eA802717bFb9833400264Dd12c2bCeAa34a6d";
  };

  environment.etc."ethos-help".text = ''

            ████████ ██████████ ██      ██   ███████    ████████
           ░██░░░░░ ░░░░░██░░░ ░██     ░██  ██░░░░░██  ██░░░░░░
           ░██          ░██    ░██     ░██ ██     ░░██░██
           ░███████     ░██    ░██████████░██      ░██░█████████
           ░██░░░░      ░██    ░██░░░░░░██░██      ░██░░░░░░░░██
           ░██          ░██    ░██     ░██░░██     ██        ░██
           ░████████    ░██    ░██     ░██ ░░███████   ████████
           ░░░░░░░░     ░░     ░░      ░░   ░░░░░░░   ░░░░░░░░

          Version 1 ("At Least It's An ETHOS"; signature edition)
           Ethereum distribution of GNU/Linux/NixOS by DappHub

  '';
}
