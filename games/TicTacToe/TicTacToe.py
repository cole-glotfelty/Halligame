#!/usr/bin/env python

# Tic-Tac-Toe Game Module for Halligame Testing
# Written on 2025-03-22 by Cole Glotfelty
# Last edited by: Cole Glotfelty 2025-03-22


# TODO: Get scoring working using math
# TODO: Potentially make a player class/struct for storing player related info

# Potential Framework structure for python
# each game has a Game class and a Player class
# The game class implements the rules and game specific data
# The Player class stores player specific data (potentially part of the Game constructor)

class TicTacToe:
    def __init__(self):
        self.board = [[" " for _ in range(3)] for _ in range(3)]
        self.currentPlayer = 0
        # Sum to 15 and you win
        # 8 | 1 | 6
        # ---------
        # 3 | 5 | 7
        # ---------
        # 4 | 9 | 2
        self.__boardFull = 0
        self.__playersScore = [0] * 2
        self.__scoreMap = {
                0: 8,
                1: 1,
                2: 6,
                3: 3,
                4: 5,
                5: 7,
                6: 4,
                7: 9,
                8: 2
                }

    # Game Logic
    def run(self):
        while self.__boardFull != 9 or 15 not in self.__playersScore:
            self.__renderBoard()
            # Tuple of current player as well as their move
            event = (self.currentPlayer, self.__userTurn())
            if self.validEvent(event):
                self.Update(event)
            self.currentPlayer = (self.currentPlayer + 1) % 2
            print(self.board)

    ### Client Side ###
    def __renderBoard(self):
        for row in self.board:
            print("-------------")
            print("|   |   |   |")
            print(f"| {row[0]} | {row[1]} | {row[2]} |")
            print("|   |   |   |")
        print("-------------\n")
        print("Squares are enumerated left to right, top to bottom, from 1 through 9")

    def __userTurn(self):
        return int(input("Which square do you want? ")) - 1

    ### Server Side ###
    def Update(self, event):
        (currentPlayer, move) = event
        self.board[move // 3][move % 3] = 'X' if currentPlayer else 'O'
        self.__boardFull += 1
        self.__playersScore[currentPlayer] += self.__scoreMap[move]
        print(self.__playersScore)
        print(self.__boardFull)

    def validEvent(self, event) -> bool:
        (_, move) = event
        if 0 <= move and move < 9:
            return self.board[move // 3][move % 3] == " "
        else:
            print("Enter a number between 1 and 9.")
            self.currentPlayer += 1


if __name__ == "__main__":
    game = TicTacToe()
    game.run()
