# Halligame
A framework for running multiplayer games concurrently.

# System Architecture
![](./topology/HalligameTopology.drawio.svg)

## Game Module Architecture
Each game module for halligame is structured as a sub-package of the games
sub-package. The modules are stored in the games directory. Each game module 
should aim to follow the same uniform interface so that the game server and
validation server setup can be automated. The interface is as follows:

![](./topology/ClassDiagrams.drawio.svg)

### Required Implementation for Adding Games
#### Functions that are required to be implemented in gameServer.py


#### Functions that are required to be implemented in gameClient.py


### Exported Functions available to Games

### Screen (halligame.utils.screen)**
- `__init__(gotInputFunc, width, height)` : Initializes the screen class. Takes 
    a callback function gotInputFunc that has signature gotInputFunc(input) 
    where input is the input from the user. Input is either a normal char or 
    a special character, which is handled by 
    [ncurses curses.KEY_* constants](https://docs.python.org/3/library/curses.html#constants). 
    Additionally, takes the desired width and height of the virtual screen to 
    print to.
- `write(row, col, toPrint)` : Prints the contents of toPrint starting at 
    (row, col) to the screen. toPrint must be convertible to string. Updates 
    made when refresh is called.
- `print(toPrint, end="\n")` : Prints the contents of toPrint, starting at the 
    bottom left corner as if it were a normal terminal. end is appended to the 
    end of toPrint before printing. toPrint must be convertible to string. 
    Updates made when refresh is called.
- `clearScreen()` : Removes everything from the screen. Updates made when 
    refresh is called.
- `refresh()` : Refreshes the screen, making all pending changes visible to the 
    user
- `shutdown()` : Must be called when the client is finished displaying to the 
    terminal. Closes the virtual window and restores the terminal appearance 
    to its normal state


# Development
Dependencies: rebar3, uv

You will first have to run the following inorder to install the submodules.

```bash
$ git submodule update --init
```
From here, you should be able to use `uv run {file}.py` to activate the venv for
that run of the python file.

## For Nix/Devenv Users
There is a provided `devenv.nix` file which should enforce that all dependencies
are installed properly provided you have devenv set up.


