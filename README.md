# Halligame
A framework for running multiplayer games concurrently.

# System Architecture
![](./topology/HalligameTopology.drawio.svg)

## Game Module Architecture
Each game module for halligame is structured as a sub-package of the games
sub-package. The modules are stored in the games directory. Each game module 
should aim to follow the same uniform interface so that the game server and
validation server setup can be automated. The interface is as follows:

### For Turn Based Games

**Game**
- drawScreen(self, ...) : this is what will draw the screen for the clients
- eventIsValid(self, event) : this is what runs on the validation server it 
takes an event from a player and returns an response on weather the move was
valid.
- eventUpdate(self, event) : update the game state with the event presuming it's
valid, but it does not check

**Player**
- takeTurn() : any input the player needs to give the game is requested here

# Development
There is a provided `devenv.nix` file which should enforce that all dependencies
are installed properly provided you have devenv set up.
