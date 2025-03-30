{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    git
  ];

  languages.python = {
    enable = true;
    version = "3.11";
    venv = {
      enable = true;
      requirements = ''
        prompt-toolkit
        erpy
      '';
    };
  };
}
