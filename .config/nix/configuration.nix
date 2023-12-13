{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  hardware.deviceTree = {
    enable = true;
    filter = "*rpi*.dtb";
    overlays = [{
      name = "spi";
      dtsFile = ./spi.dts;
    }];
  };

users.groups.spi = {};

services.udev.extraRules = ''
  SUBSYSTEM=="spidev", KERNEL=="spidev0.0", GROUP="spi", MODE="0660"
'';
  
  time.timeZone = "Europe/Uzhgorod";

  networking.hostName = "alarmpi";
  
  users.users.nazar = {
    isNormalUser = true;
    description = "Nazar Klovanych";
    home = "/home/nazar";
    extraGroups = ["users" "wheel"];
    openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJAhZ6wg+6tHLPXOiMnvDsf7jd/N6RbzEaJaJa0ElL3F n.klovanych@atwix.com"];
  };
  
  fileSystems."/home/nazar/shared_storage" = 
    { device = "/dev/sda1";
      fsType = "ext4";
    };

  environment.systemPackages = with pkgs; [
     emacs
     syncthing
     samba
     wget
  ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  
  networking.firewall.allowedTCPPorts = [
    445 139 22
  ];
  networking.firewall.allowedUDPPorts = [
    445 139
  ];


  services.syncthing = {
    enable = true;
    user = "nazar";
    dataDir = "/home/nazar/syncthing";
    configDir = "/home/nazar/.config/syncthing";
    overrideDevices = true;
    overrideFolders = true;
    devices = {
      "nazarPhone" = {id = "YUFX7N7-3FGLAF3-XEJ3GIY-HOTQUML-OQJRURL-5QIOPHV-6XOIMW2-DOR7DQW";};
      "oksanaPhone" = {id = "KOFWPTV-6RNNS6F-ZDHJOVB-DHPEEPI-2P3MQXY-DLPVF2W-RGF5YSR-FBQYDAL";};
    };
    
    folders = {
      "nazarsPhone" = {
        "path" = "/home/nazar/shared_storage/nazarPhone";
        "devices" = ["nazarPhone"];
      };
      "oksanaPhone" = {
        "path" = "/home/nazar/shared_storage/OksanaPhone";
        "devices" = ["oksanaPhone"];
      };
    };
  };
  
  services.samba = {
    enable = true;
    enableNmbd = true;
    securityType = "user";
     extraConfig = ''
      workgroup = WORKGROUP
      server string = smbnix
      netbios name = smbnix
      security = user
      guest account = nobody
      map to guest = bad user
   '';
    shares = {
      alarmpi = {
        path = "/home/nazar/shared_storage";
        browseavle = "Yes";
        "read only" = "no";
        "guest ok" = "Yes";
        "create mask" = "0664";
        "directory mask" = "0755";
        "force user" = "nazar";
        "force group" = "users";
      };
    };
    
  };

  system.stateVersion = "22.11"; # Did you read the comment?

}

