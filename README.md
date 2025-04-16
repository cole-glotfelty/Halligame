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

## Required Implementation for Adding Games
#### Functions that are required to be implemented in gameServer.py
- `__init__(comms)` : Called automatically when the server is started. comms is an instance of the ServerCommunicate class and gives the game server access to the public ServerCommunicate functions (documented below)
- `addClient(ClientPid)` : Called automatically by the client when a new client node joins. Should ideally call `confirmJoin(ClientPid, Message)` to confirm the request.
- `removeClient(ClientPid)` : Called when the game client calls the function `ClientComms.shutdown()`
- `gotClientMessage(ClientPid, Message)` : Called when the game client calls `sendMessage(Message)`
- `otherMessageType(ClientPid, Message)` : Called when the server receives a message from a game client that has an unidentified header (none of the above)

#### Functions that are required to be implemented in gameClient.py
- `__init__(comms)` : Called automatically when the client is started. comms is an instance of the ClientCommunicate class and gives the game client access to the public ClientCommunicate functiosn (documented below)
- `updateState(newState)` : Called with the provided state when the game server calls `broadcastState(state)`
- `gotServerMessage(Message)` : Called on the particular client node when the game server calls `sendClientMessage(ClientPid, Message)`
- `confirmedJoin(Message)` : Called when the game server responds to addClient by calling `confirmJoin(ClientPid, Message)`

### Exported Functions available to Games

#### Functions exported by ServerComms
- `broadcastState(State)` : Sends the provided state to all clients connected to the server, with `updateState(newState)` being called when the client receives the message. This assumes the usage of the provided gameState.py module.
- `broadcastMessage(Message)` : Sends the provided message to all connected clients, with `gotServerMessage(Message)` being called in each client with the message.
- `confirmJoin(ClientPid, Message)` : When called by the server, confirmedJoin(Message) is called on the client node associated with ClientPid 
- `sendClientMessage(ClientPid, Message)` : Sends a message to a particular client, with the `gotServerMessage(Message)` function being called when the client receives it
- `shutdown()` : Should be called when the game is over and the server should be shut down

#### Functions exported by ClientComms
- `sendMessage(Message)` : Sends a message to the server, with `gotClientMessage(ClientPid, Message)` being called when the server receives the message
- `shutdown()` : Should be called when the client leaves (or the game is over)

### Screen (halligame.utils.screen)**
- `Screen(gotInputFunc, gotClickFunc, gotMouseClickFunc)` : 
    Initializes the screen class. Takes a callback function gotInputFunc that 
    has signature gotInputFunc(input) where input is the input from the user. 
    Input is either a normal char or a special character, which is handled by 
    [ncurses curses.KEY_* constants](https://docs.python.org/3/library/curses.html#curses.KEY_MIN). 
    Additionally, takes another callback function gotMouseClickFunc that is 
    called when the user left clicks on the screen. gotMouseClickFunct 
    should have signature `gotMouseClickFunc(row, col, region)` where row and 
    col are the row and column of the screen respectively, and region is the 
    region of the mouse click (as created by `addClickableRegion`) or None 
    if the click was not in a region.
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
- `addColor(r, g, b, colorId)` : Add a new color to the palette, where 
    r, g, and b are integers between 0 and 256 referring to the intensity of 
    the color. Predefined colors are black, blue, cyan, green, magenta, red, white, yellow
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


