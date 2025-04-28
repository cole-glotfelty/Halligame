# Halligame
A framework for running multiplayer games concurrently.

# Using Halligame
If you are a Tufts student with access to Halligan, you can run the following to
add `hg` to your path.

```bash
$ source /h/mdanie09/Public/hg.sh # For Bash/ZSH
```

```csh
$ source /h/mdanie09/Public/hg.csh # For CSH
```

Now run `hg` for usage.

# System Architecture
![](./topology/HalligameTopology.drawio.svg)

## Game Module Architecture
Each game module for halligame is structured as a sub-package of the games
sub-package. The modules are stored in the games directory. Each game module 
should aim to follow the same uniform interface so that the game server and
validation server setup can be automated. The interface is as follows:

![](./topology/ClassDiagrams.drawio.svg)

# Addiing your Own Game to Halligame
Below is a list  of functions that must be implemented in order for your game
to work with the Halligame framework. For an example of what this looks like
in the [Example Game](./src/halligame/games/ExampleGame/). Additionally, you
will need to add your game to the `__init_.py` in in src/halligame/games for it 
to show in the CLI. From here, you can use the CLI `hg` to run and test your 
game. Just note that it will not run until you have implemented all functions
found in the required section below.

## Required Implementation for Adding Games
#### Functions that are required to be implemented in gameServer.py
- `__init__(comms)` : Called automatically when the server is started. comms is 
an instance of the ServerCommunicate class and gives the game server access to 
the public ServerCommunicate functions (documented below)
- `addClient(ClientPid)` : Called automatically by the client when a new client 
node joins. Should call `confirmJoin(ClientPid, Message)` to confirm the request.
- `removeClient(ClientPid)` : Called when the game client calls the function 
`ClientComms.shutdown()`
- `gotClientMessage(ClientPid, Message)` : Called when the game client calls 
`sendMessage(Message)`

#### Functions that are required to be implemented in gameClient.py
- `__init__(comms)` : Called automatically when the client is started. comms is 
an instance of the ClientCommunicate class and gives the game client access to 
the public ClientCommunicate functiosn (documented below)
- `updateState(newState)` : Called with the provided state when the game server 
calls `broadcastState(state)`
- `gotServerMessage(Message)` : Called on the particular client node when the 
game server calls `sendClientMessage(ClientPid, Message)`
- `joinConfirmed(Message)` : Called when the game server responds to addClient 
by calling `confirmJoin(ClientPid, Message)`

### Exported Functions available to Games

#### Functions exported by ServerComms
- `broadcastState(State)` : Sends the provided state to all clients connected 
to the server, with `updateState(newState)` being called when the client 
receives the message. This assumes the usage of the provided gameState.py module.
- `broadcastMessage(Message)` : Sends the provided message to all connected 
clients, with `gotServerMessage(Message)` being called in each client with 
the message.
- `confirmJoin(ClientPid, Message)` : When called by the server, 
`joinConfirmed(Message)` is called on the client node associated with ClientPid 
- `sendClientMessage(ClientPid, Message)` : Sends a message to a particular 
client, with the `gotServerMessage(Message)` function being called when the 
client receives it
- `shutdown()` : Should be called when the game is over and the server should be 
shut down

#### Functions exported by ClientComms
- `sendMessage(Message)` : Sends a message to the server, with 
`gotClientMessage(ClientPid, Message)` being called when the server receives 
the message
- `shutdown()` : Should be called when the client leaves (or the game is over)

### Screen (halligame.utils.screen)
- `Screen(gotInputFunc, gotMouseClickFunc)` : 
    Initializes the screen class. Takes a callback function gotInputFunc that 
    has signature `gotInputFunc(input)` where input is the input from the user. 
    Input is either a normal char or a special character, which is handled by 
    [ncurses curses.KEY_* constants](https://docs.python.org/3/library/curses.html#curses.KEY_MIN). 
    Additionally, takes another callback function gotMouseClickFunc that is 
    called when the user clicks on the screen. gotMouseClickFunc 
    should have signature `gotMouseClickFunc(row, col, region, mouseEventType)` 
    where row and col are the row and column of the screen respectively, and 
    region is the region of the mouse click (as created by 
    `addClickableRegion`) or None if the click was not in a region. 
    mouseEventType is the type of the mouse event, either "left_click", 
    "right_click", "middle_click", or "other"
- `write(row, col, toPrint)` : Prints the contents of toPrint starting at 
    (row, col) to the screen. toPrint must be convertible to string. Updates 
    made when refresh is called.
- `print(toPrint, end="\n")` : Prints the contents of toPrint, starting at the 
    bottom left corner as if it were a normal terminal. end is appended to the 
    end of toPrint before printing. toPrint must be convertible to string. 
    Updates made when refresh is called.
- `displayFullScreenMessage(message, font=None)` : Displays the contents of 
    message centered vertically and horizontall in the terminal window. 
    Clears and refreshes the screen.
- `clearScreen()` : Removes everything from the screen. Updates made when 
    refresh is called.
- `refresh()` : Refreshes the screen, making all pending changes visible to the 
    user

- `terminalHeight()` : get the height of the terminal in pixels
- `terminalWidth()` : get the width of the terminal in pixels
- `getCenteredRow(toPrint)` : Takes toPrint, which must either be a string or 
    something convertible to a string. Returns the row number where if you 
    called write(toPrint), it would be centered vertically in the screen. 
    If the contents is taller than the screen, returns 0.
- `getCenteredCol(toPrint)` : Similar to getCenteredRow. Takes toPrint, which 
    must either be a string or something convertible to a string. Returns the 
    column number where if you called write(toPrint), it would be centered 
    horizontally in the screen. If the contents is wider than the screen, 
    returns 0.

- `addColor(r, g, b, colorId)` : Add a new color to the palette, where 
    r, g, and b are integers between 0 and 256 referring to the intensity of 
    the color. Predefined colors are black, blue, cyan, green, magenta, red, 
white, yellow
- `addColorPair(foreground, background, pairId)` : Takes in two color IDs and 
    defines a new color pair with ID pairId. A color pair is two colors, 
    where the foreground color is the color of the text on the screen, and the 
    background is the color of the screen behind the text (the highlight color)
- `setStyle(colorPairId)` : Takes a color pair ID and sets the style of the 
    terminal to the color pair defined by that ID, meaning the the background 
    of the terminal is now the background color of that color pair and all 
    printing will now default to that color pair.
- `addClickableRegion(row, col, height, width, id)` : Add a clickable region 
    to the screen. The region starts in the top left at (row, col) and is 
    height pixels tall and width pixels wide. When calling the callback 
    gotMouseClickFunc, if the click is within this region, then the region 
    argument is set to id. Clicks in overlapping regions are decided based on 
    which region was defined more recently.
- `clearClickableRegions()` : Remove all clickable regions from the screen

- `shutdown()` : Must be called when the client is finished displaying to the 
    terminal. Closes the virtual window and restores the terminal appearance 
    to its normal state

# Development
Dependencies: python 3.10+, rebar3, uv

After pulling, you will first have to run the following in order to install 
the submodules.

```bash
$ git submodule update --init
```

Once the submodules are pulled and installed, you can simply run make to install 
dependencies and  build the python and erlang modules.

```bash
$ make
```

The last step is enabling the development CLI. To do this first export `HG_ROOT`

```bash
$ export HG_ROOT=$(pwd) # when in the top level of Halligame
```

Then activate the CLI:

```bash
$ . src/cli/activate_dev.sh # for Bash/ZSH
```

```csh
$ . src/cli/activate_dev.csh # for csh/tcsh
```

Now that everything is complied, you're ready to begin. You can run `hg` for
usage.

## For Nix/Devenv Users
There is a provided `devenv.nix` file which should enforce that all dependencies
are installed and exposed properly provided you have devenv set up.

## File Structure Overview – Where to Find Everything!
**Top Level**
- Makefile: run make to build/compile all erlang modules with rebar3 and update
python modules
- README.md: The thing you're reading right now that explains what's going on
- devenv.nix: ~~For Brennan~~. If you're using nix devenv, this will 
automatically load all requirements and run make on entering the directory.
- pyproject.toml: Configuration of dependencies and build order for uv/python

**src/cli**
- activate_dev.sh: Script to expose `hg` command to users with Bash/ZSH shells.
For development purposes only, this allows you to define a separate path for 
where to source files from.
- activate_dev.csh: Script to expose `hg` command to users with csh/tcsh shells.
For development purposes only, this allows you to define a separate path for 
where to source files from.
- background.py: Allows for erlang messages to be sent and recieved from the
server broker, in the background.
- cli.py: command line interface which calls/sends erlang functions/messages
- sh.activate: the same as `activate_dev.sh` but sets the path of halligame 
source code to production which is stored in Michael's public folder.
- csh.activate: the same as `activate_dev.csh` but sets the path of halligame 
source code to production which is stored in Michael's public folder.
- hg_background.bash: helper to launch the `background.py` process.

**src/cli/src**
- handleCLIRequest.erl: Handles certain requests coming from the "hg" command 
line tool.

**src/halligame/games**
- Canvas: directory, contains a gameServer.py and a gameClient.py representing 
the server and client of the Canvas game.
- ExampleGame: directory, contains an example gameServer.py and gameClient.py
this is not a playable game, but rather a barebones example/template to copy
when making additional games.
- TicTacToe: directory, contains a gameServer.py and a gameClient.py 
representing the server and client of TicTacToe.
- Uno: directory, contains a gameServer.py and a gameClient.py representing the 
server and client of Uno.

**src/halligame/utils**
- ServerComms.py: Module to facilitate communication from the game server to 
the client.
- clientComms.py: Communication Module for clients to communicate with the 
game server.
- misc.py: A collection of various functions used occasionally.
- gameClientTemplate.py: Super class to be used when making a gameClient to 
enforce required functions
- gameServerTemplate.py: Super class to be used when making a gameServer to 
enforce required functions
- gameState.py: Serializes and deserializes game state for client/server 
communication.
- screen.py: Utility for drawing and writing to terminals using NCurses.

**src/serverbroker**
- serverbroker.erl: Keeps track of who's online and who's playing what.
- serverbroker_app.erl: Implements the application behavior for serverbroker.
- serverbroker_sup.erl: Implements a top-level supervisor for serverbroker.
