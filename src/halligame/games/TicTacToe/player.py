# Tic-Tac-Toe Player Module for Halligame Testing
# Written on 2025-03-22 by Cole Glotfelty
# Last edited by: Cole Glotfelty 2025-03-22


class Player:
    def __init__(self):
        # (x,y) tuple representing the 3x3 grid
        # X Y| 0 | 1 | 2
        # 0 |___|___|___
        # 1 |___|___|___
        # 2 |___|___|___
        self.selectedTiles = []

    def takeTurn(self) -> int:
        """
        Allows the player to interact with the game.

        In the case of Tic-Tac-Toe, the player can select a tile from the grid,
        validation that the tile selection is a number within range occurs here.
        """
        # TODO: Try/Catch here?
        playerInput = int(input("Select square [1..9]: "))
        while not (1 <= playerInput and playerInput <= 9):
            print("Please enter a number between 1 and 9!")
            playerInput = int(input("Select square [1..9]: "))
        return playerInput - 1
