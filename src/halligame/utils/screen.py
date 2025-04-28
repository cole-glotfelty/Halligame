"""Wraw to the screen using ncurses.

Written by: Will Cordray
"""

import curses
import subprocess
import sys
import threading
import time
from collections.abc import Callable
from typing import Any

import pyfiglet

class Screen:
    """Represents -- you guessed it -- a screen."""

    def __init__(
        self,
        gotInputFunc: Callable[[str], None],
        gotMouseClickFunc: Callable[[int, int, Any, str], None],
    ) -> None:
        """Initializes the class.

        gotInputFunc is called when receiving input
        gotMouseClickFunc is called when a mouse click is recieved.
        """
        #: Called when receiving input. The input is one character.
        self.__gotInput: Callable[[str], None] = gotInputFunc
        #: Called when the mouse has been clicked.
        #: The input is the row, the column, the region (as defined by the 
        # user), and the type of the event (eg. left_click, etc.)
        self.__gotMouse: Callable[[int, int, Any, str], None] = (
            gotMouseClickFunc
        )
        #: Protects internal state.
        self.__lock = threading.Lock()
        #: The curses window.
        self.__stdscr: curses.window

        self.__initCurses()

        #: Whether color is supported on the current terminal.
        self.__colorSupport = curses.COLORS >= 8
        #: Whether extended color support is availible on this terminal.
        self.__extendedColorSupport = curses.COLORS >= 128

        self.__stdscr.clear()

        #: The thread that monitors user input.
        self.__monitorInputThread = threading.Thread(
            target=self.__monitorInput, args=[]
        )
        self.__monitorInputThread.daemon = True  # kill thread when main done
        self.__monitorInputThread.start()

        try:
            self.__stdscr.move(
                self.__terminalHeight(), 0
            )  # set starting printing loc
        except Exception:  # terminal window is being weird
            pass

        #: The list of colors.
        self.__colors = {
            "black": curses.COLOR_BLACK,
            "blue": curses.COLOR_BLUE,
            "cyan": curses.COLOR_CYAN,
            "green": curses.COLOR_GREEN,
            "magenta": curses.COLOR_MAGENTA,
            "red": curses.COLOR_RED,
            "white": curses.COLOR_WHITE,
            "yellow": curses.COLOR_YELLOW,
        }

        #: The dictionary of defined foreground background color pairs. 
        # Maps IDs to the user defined pair
        self.__colorPairs = {}
        #: The next colorID to use when the user defines a new color
        self.__nextColorID = 10
        #: The next colorPairID to use when the user defines a new color
        self.__nextColorPairID = 10
        #: All of the clickable regions defined by the user
        self.__clickableRegions = []

        # give screen time to set up (instantaneous printing causes weird bugs)
        time.sleep(0.1)

    def __initCurses(self) -> None:
        """Initialize curses."""
        # turn the terminal into a curses window
        self.__stdscr = curses.initscr()
        curses.start_color()  # enable color support
        curses.use_default_colors()  # keep the curr colors of the terminal
        curses.mousemask(curses.ALL_MOUSE_EVENTS)  # enable mouse support
        curses.noecho()  # prevent user input from appearing on screen
        curses.cbreak()  # get key input before user types [enter]

        curses.curs_set(0)  # make the cursor invisible

        self.__stdscr.keypad(
            True
        )  # enable support for special keys like up-arrow.

    def shutdown(self) -> None:
        """Shut down curses. DON'T FORGET TO CALL!!!!!"""
        curses.nocbreak()
        self.__stdscr.keypad(False)
        curses.echo()

        curses.curs_set(2)  # make the cursor visible again

        curses.endwin()

        if self.__colorSupport:
            # Needed to reset colors to the old state.
            subprocess.run(["reset"], stdout=sys.stdout)

    # Note that .getch() also refreshes the screen
    def __monitorInput(self) -> None:
        """Monitor thread that checks for and processes user input."""
        window = curses.newwin(self.terminalHeight(), self.terminalWidth())
        window.keypad(True)  # enable support for special keys like up-arrow.
        while True:
            c = window.getch()  # this blocks until input

            if c == curses.KEY_MOUSE:
                (_, mcol, mrow, _, bstate) = curses.getmouse()

                regionId = self.__findRegion(mrow, mcol)
                mouseEventType = self.__getMouseEventType(bstate)
                self.__gotMouse(mrow, mcol, regionId, mouseEventType)
            else:
                if type(c) is int:  # if int, try to convert to char
                    try:
                        c = chr(c)
                    except ValueError:  # other code, so ignore
                        pass
                self.__gotInput(c)

    def __findRegion(self, mrow: int, mcol: int) -> Any:
        """Return the region of the row and column (or None)."""
        with self.__lock:
            # reverse the list to check more recently defined regions first
            for row, col, height, width, id in reversed(
                self.__clickableRegions
            ):
                if (
                    mrow >= row
                    and mrow < row + height
                    and mcol >= col
                    and mcol < col + width
                ):
                    return id
        return None

    def __getMouseEventType(self, bstate: bytes) -> str:
        """Determines what the type of the mouse click was."""
        # if left click
        if (
            bstate & curses.BUTTON1_PRESSED != 0
            or bstate & curses.BUTTON1_CLICKED != 0
        ):
            return "left_click"
        elif (
            bstate & curses.BUTTON2_PRESSED != 0
            or bstate & curses.BUTTON2_CLICKED != 0
        ):
            return "middle_click"
        elif (
            bstate & curses.BUTTON3_PRESSED != 0
            or bstate & curses.BUTTON3_CLICKED != 0
        ):
            return "right_click"
        else:
            return "other"

    def terminalHeight(self) -> int:
        """Return the height of this terminal."""
        with self.__lock:
            return self.__terminalHeight()

    def terminalWidth(self) -> int:
        """Return the width of this terminal."""
        with self.__lock:
            return self.__terminalWidth()

    def __terminalHeight(self) -> int:
        """Return the height of this terminal."""
        rows, _cols = self.__stdscr.getmaxyx()
        return rows

    def __terminalWidth(self) -> int:
        """Return the width of this terminal."""
        _rows, cols = self.__stdscr.getmaxyx()
        return cols

    def clearScreen(self) -> None:
        """Clears the screen (removes all text)."""
        with self.__lock:
            self.__stdscr.clear()
            self.__stdscr.move(
                self.__terminalHeight() - 1, 0
            )  # move cursor to bottom left

    #
    def write(self, row: int, col: int, toPrint: Any, colorPairId: Any = None) -> None:
        """Prints a string to the screen, starting at (row, col).

        The row and column is the starting position of where to print toPrint
        (which must be convertible to a string). The colorPairID is the 
        user defined ID for what color pair to use (optional).
        """
        with self.__lock:
            if not self.__colorSupport or colorPairId not in self.__colorPairs:
                colorPairId = None

            (prevRow, prevCol) = self.__stdscr.getyx()  # save cursor position

            printing = str(toPrint)

            lines = printing.split("\n")
            for i in range(len(lines) - 1):
                self.__write(row + i, col, lines[i], colorPairId)
            self.__write(row + len(lines) - 1, col, lines[-1], colorPairId)

            self.__stdscr.move(prevRow, prevCol)  # reset cursor position

    def __write(
        self, row: int, col: int, printing: str, colorPairId: Any=None
    ) -> None:
        """Helper function that writes a single line to the screen."""
        try:
            if colorPairId is None:
                self.__stdscr.addstr(row, col, printing)
            else:
                colorPairCode = self.__colorPairs[colorPairId]
                self.__stdscr.addstr(
                    row, col, printing, curses.color_pair(colorPairCode)
                )
        except curses.error:
            pass  # ignore out of bounds characters # TODO

    # Note that this is a debug function and should not be used in production
    def print(self, toPrint: Any, end:str="\n") -> None:
        """Tries to emulate terminal printing."""
        with self.__lock:
            printing = str(toPrint)

            printing += end

            # need to print lines one at a time because of bug in ncurses:
            # "A bug in ncurses, the backend for this Python module,
            # can cause SegFaults when resizing windows. This is
            # fixed in ncurses-6.1-20190511. If you are stuck with an
            # earlier ncurses, you can avoid triggering this if you do
            # not call addstr() with a str that has embedded newlines.
            # Instead, call addstr() separately for each line."
            lines = printing.split("\n")
            for i in range(len(lines) - 1):
                self.__print(lines[i], newline=True)
            self.__print(lines[-1], newline=False)

    # private helper
    def __print(self, printing: str, newline: bool) -> None:
        """Helper function for print that prints a line to the screen."""
        try:
            self.__stdscr.addstr(printing)

            if newline:
                # move up one line, and reset cursor to bottom left
                self.__stdscr.move(0, 0)  # so deleteln deletes the top line
                self.__stdscr.deleteln()
                self.__stdscr.move(self.__terminalHeight() - 1, 0)
        except curses.error:
            # reset cursor position to correct loc
            self.__stdscr.move(self.__terminalHeight() - 1, 0)

    def refresh(self) -> None:
        """Refreshes the window (sends updates to screen)."""
        with self.__lock:
            if self.__stdscr.is_wintouched():  # only refresh if touched
                self.__stdscr.refresh()

    # rgb between 0 and 1000
    def addColor(self, r: int, g: int, b: int, colorId: Any) -> None:
        """Defines a new color.

        Adds a new color to the screen with the given rgb values (0-255), 
        and assigns that color the user supplied colorId to use it later.
        """
        if not self.__extendedColorSupport:
            return
        with self.__lock:
            self.__colors[colorId] = self.__nextColorID
            r = self.__scaleColor(r)
            g = self.__scaleColor(g)
            b = self.__scaleColor(b)

            curses.init_color(self.__nextColorID, r, g, b)

            self.__nextColorID += 1

    def __scaleColor(self, color:int|float) -> int:
        """Scales a color from between 0 and 255 to between 0 and 1000."""
        return min(max(int(color * (1000.0 / 255.0)), 0), 1000)

    def addColorPair(self, foreground: Any, background: Any, pairId: Any) -> None:
        """Defines a new color pair.
        
        Takes two colorIds for the foreground color and background color and 
        creates a new color pair, assigning it the id pairId.
        """
        if not self.__extendedColorSupport:
            return
        with self.__lock:
            self.__colorPairs[pairId] = self.__nextColorPairID

            fgColorCode = self.__colors[foreground]
            bgColorCode = self.__colors[background]

            curses.init_pair(self.__nextColorPairID, fgColorCode, bgColorCode)

            self.__nextColorPairID += 1

    def setStyle(self, colorPairId: Any) -> None:
        """Sets the style of the terminal to the colors of colorPairId."""
        with self.__lock:
            if not self.__colorSupport or colorPairId not in self.__colorPairs:
                return
            colorPairCode = self.__colorPairs[colorPairId]

            self.__stdscr.bkgd(" ", curses.color_pair(colorPairCode))

    def addClickableRegion(
        self, row: int, col: int, height: int, width: int, id: Any
    ) -> None:
        """Defines a new clickable region.
        
        Takes the row and column of the top left point of the region box, as 
        well as the height and width of the box, and creates a new region with 
        id id. If the user clicks within that box, the gotMouseClickFunc will 
        be supplied the id of the region that it is contained within (or None 
        if it is not in a region).
        """
        with self.__lock:
            self.__clickableRegions.append((row, col, height, width, id))

    def clearClickableRegions(self) -> None:
        """Removes all previously defined clickable regions."""
        with self.__lock:
            self.__clickableRegions = []

    def getCenteredRow(self, toPrint: Any) -> int:
        """Take something to print and return the row to print at to center."""
        with self.__lock:
            row = (self.__terminalHeight() // 2) - (
                len(str(toPrint).split("\n")) // 2
            )
            return max(0, row)

    def getCenteredCol(self, toPrint: Any) -> int:
        """Take something to print and return the col to print at to center."""
        with self.__lock:
            toPrintSplit = str(toPrint).split("\n")

            if len(toPrintSplit) > 0:
                col = (self.__terminalWidth() // 2) - (
                    len(toPrintSplit[0]) // 2
                )
            else:
                col = self.__terminalWidth() // 2

            return max(0, col)

    def displayFullScreenMessage(self, message: Any, font: str=None) -> None:
        """Writes a full screen message.
        
        Clears and refreshes the screen, printing the message so that it is 
        centered in the screen.
        """
        self.clearScreen()

        if font is not None:
            message = pyfiglet.figlet_format(
                message, font=font, width=self.terminalWidth() - 10
            )

        row = self.getCenteredRow(message)
        col = self.getCenteredCol(message)

        self.write(row, col, message)
        self.refresh()
