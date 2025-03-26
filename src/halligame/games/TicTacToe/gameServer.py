#!/usr/bin/env python

# this is just the very basic structure of the server. Obviously more is needed


class TicTacToeSever:
    # comms is the function to call when you want to send a message to the server
    def __init__(self, comms):
        pass # add what is needed

    
    # This is the function that is called when the server receives a message 
    # from one of the clients, most likely an event/move. Note that I haven't 
    # quite figured out the erlang side of things to determine which player 
    # sent the message, but that is obviously coming.
    def eventIsValid(self, event):
        pass
