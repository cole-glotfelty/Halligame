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
    # export HG_ROOT=$(pwd)
    #
    # HGSHELLPROC=$(ps -o ppid= -p $$)
    # HGTTY=$(tty)
    #
    # alias hg="$HG_ROOT/.venv/bin/python $HG_ROOT/src/cli/cli.py"
    # $HG_ROOT/.venv/bin/python $HG_ROOT/src/cli/background.py \
    #     "$HGSHELLPROC" "$HGTTY" &
    # disown
  '';
}
