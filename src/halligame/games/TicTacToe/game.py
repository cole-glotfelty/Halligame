#!/usr/bin/env python

# Tic-Tac-Toe Game Module for Halligame Testing
# Written on 2025-03-22 by Cole Glotfelty
# Last edited by: Cole Glotfelty 2025-03-22

# Potential Framework structure for python
# each game has a Game class and a Player class
# The game class implements the rules and game specific data
# The Player class stores player specific data (potentially part of the Game constructor)

from player import Player


class TicTacToe:
    def __init__(self):
        # TODO: Should this be wrapped into a single data structure?
        self.board = [[3 * y + x + 1 for x in range(3)] for y in range(3)]

        self.__currentPlayer = 0
        self.__gameOver = False
        self.__boardFull = 0
        self.__players = [Player() for x in range(2)]

    def run(self) -> None:
        while not self.__gameOver:
            self.drawScreen()
            event = (
                self.__currentPlayer,
                self.__players[self.__currentPlayer].takeTurn(),
            )
            if self.eventIsValid(event):
                self.eventUpdate(event)

            self.__currentPlayer = (self.__currentPlayer + 1) % 2

    def drawScreen(self) -> None:
        for row in self.board:
            print("-------------")
            print("|   |   |   |")
            print(f"| {row[0]} | {row[1]} | {row[2]} |")
            print("|   |   |   |")
        print("-------------\n")

    def eventIsValid(self, event) -> bool:
        (_, move) = event
        return self.board[move // 3][move % 3] not in ["X", "O"]

    def eventUpdate(self, event) -> None:
        # TODO: Setup scoring based on player's selected squares
        valueMap = [[8, 1, 6], [3, 5, 7], [4, 9, 2]] # 3 in a row will sum to 15

        (currentPlayer, move) = event
        self.board[move // 3][move % 3] = "X" if currentPlayer else "O"
        self.__players[self.__currentPlayer].selectedTiles.append(
            (
                move // 3,
                move % 3,
            )
        )
        self.__boardFull += 1
        if self.__boardFull == 9:
            self.__gameOver = True


if __name__ == "__main__":
    game = TicTacToe()
    game.run()
