{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    git
    rebar3
  ];

  languages.rust.enable = true;
  languages.erlang = {
    enable = true;
    package = pkgs.erlang_26;
  };
  languages.python = {
    enable = true;
    version = "3.11";
    uv = {
      enable = true;
      sync.enable = true;
    };
    venv = { enable = true; };
  };

  enterShell = ''
    git submodule update --init
    make
  '';
}
