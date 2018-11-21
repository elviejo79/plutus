{ config, pkgs, options,... }:
let 
  plutusSrc = builtins.fetchGit {
    url = "https://github.com/input-output-hk/plutus.git";
    ref = "playground";
  };
  plutus = import plutusSrc { inherit config; };
in
{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;

  nix = {
      maxJobs = 8;
      buildCores = 0;
      nixPath = options.nix.nixPath.default ++ [
        "nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz"
      ];
      binaryCaches = [ https://hydra.iohk.io https://cache.nixos.org https://mantis-hydra.aws.iohkdev.io/ ];
      requireSignedBinaryCaches = false;
      extraOptions = ''
        build-cores = 8
        auto-optimise-store = true
      '';
      trustedBinaryCaches = [ https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io/ ];
      binaryCachePublicKeys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      gc.automatic = true;
      gc.options = "--delete-older-than 7d";
    };

  networking.timeServers = [ "1.amazon.pool.ntp.org" "2.amazon.pool.ntp.org" "3.amazon.pool.ntp.org" ];

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 8080 ];
  };

  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJKNcFtDKX585wipRkoQvMxLofmoyquVRw0HoWf7zKTokc1e6G/4EpBu/klEqoIsQDCsZtkpWQU90GFc1cAnA2mvJcbJIz8efedrk6onnai/MLZjRzTAMIbjXoASK3sUXUH00W7UdKImox0nPRmmuZUk0g9lLPrt4rpWndrTOqc7H81GtxntZiQVvtjpMObBrKGaBlyt7b6P4M/x63Z55LYpUPcZ0V3ww7BD5xnop977vRvHB7fGv87jWsWlh7gXnC1p1Ykid9l7uVu0gWqZKWeNIqLIo5gCDeJLkH4crX+QLBJebs8GYrLIDqIo7PFfAXPMX7PPbGYbBgLjgH5SlN kris@MacBook-Pro"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCeUypihhOjTMIBh5JkpcY1znuuRP+42Uj76aZnmNnv1Z9C8Y46G/igEcTs8WW2jSaalVyOcjk0kLI0SM2hrbM+0lWtjeFdXLMwxG4x7OpzKr1fu5SmHHY8rhMPeChncqaRMA05NyB+qNSVgeLbOVDIBBHqotYaB1/qso77/KR/X1khiYQqgo21HXXuaNgWTwy6gsqGUEOVQI0fZ94Zjovhwc6suUMgFbmOnX8ft3BISp3tmaSIS6f+GiDFsjYcWRxvNaKUk3PHErJY/K6HUrzdFYScEcA/MLz6heyqJgj77QhbL1UjuVhHmlHHpAPtmRX6YRyCjHPBqbFTBBaKcKzX davidsmith@Davids-MacBook-Pro.local"
    ];
  };

  environment.systemPackages = with pkgs; [
    # Core
    gnumake
    file
    vim
    tmux
    wget
    rsync
    git
    nettools
    jq
    nodejs

    # Language Tools
    stack
  ];

  users.users.plutus = {
    isNormalUser = true;
    home = "/home/plutus";
    description = "Plutus user";
    extraGroups = [ "systemd-journal" ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJKNcFtDKX585wipRkoQvMxLofmoyquVRw0HoWf7zKTokc1e6G/4EpBu/klEqoIsQDCsZtkpWQU90GFc1cAnA2mvJcbJIz8efedrk6onnai/MLZjRzTAMIbjXoASK3sUXUH00W7UdKImox0nPRmmuZUk0g9lLPrt4rpWndrTOqc7H81GtxntZiQVvtjpMObBrKGaBlyt7b6P4M/x63Z55LYpUPcZ0V3ww7BD5xnop977vRvHB7fGv87jWsWlh7gXnC1p1Ykid9l7uVu0gWqZKWeNIqLIo5gCDeJLkH4crX+QLBJebs8GYrLIDqIo7PFfAXPMX7PPbGYbBgLjgH5SlN kris@MacBook-Pro"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCeUypihhOjTMIBh5JkpcY1znuuRP+42Uj76aZnmNnv1Z9C8Y46G/igEcTs8WW2jSaalVyOcjk0kLI0SM2hrbM+0lWtjeFdXLMwxG4x7OpzKr1fu5SmHHY8rhMPeChncqaRMA05NyB+qNSVgeLbOVDIBBHqotYaB1/qso77/KR/X1khiYQqgo21HXXuaNgWTwy6gsqGUEOVQI0fZ94Zjovhwc6suUMgFbmOnX8ft3BISp3tmaSIS6f+GiDFsjYcWRxvNaKUk3PHErJY/K6HUrzdFYScEcA/MLz6heyqJgj77QhbL1UjuVhHmlHHpAPtmRX6YRyCjHPBqbFTBBaKcKzX davidsmith@Davids-MacBook-Pro.local"
    ];
  };

  services.nginx = {
    enable = true;
    user = "plutus";

    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    recommendedOptimisation = true;

    appendHttpConfig = ''
      server_names_hash_bucket_size 128;
      log_format compression '$remote_addr - $remote_user [$time_local] '
                       '"$request" $status $body_bytes_sent '
                       '"$http_referer" "$http_user_agent" "$gzip_ratio"';
    '';

    virtualHosts = {
      "~." = {
        listen = [{ addr = "0.0.0.0"; port = 80; }];
        extraConfig = ''
          return 301 https://$host$request_uri;
          '';
      };
      "*.compute.amazonaws.com" = {
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:4000/";
            proxyWebsockets = true;
          };
        };
      };
    };
  };

  systemd.services.plutus-playground = {
    wantedBy = [ "nginx.service" ];
    before = [ "nginx.service" ];
    enable = true;
    path = [
      plutus.plutus-server-invoker
    ];

    serviceConfig = {
      TimeoutStartSec = "0";
      Restart = "always";
      User = "plutus";
      PrivateTmp = true;
    };

    script = "plutus-playground-server webserver -b 127.0.0.1 -p 4000 ${plutus.plutus-playground-client}";
  };
}