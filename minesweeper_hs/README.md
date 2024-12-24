# Haskell Minesweeper Game in Terminal

## How to play

1. **Start the Game**

- Run the executable in order to begin the game `stack exec minesweeper`
- The default size of the board is 8x8 which can also be modified in the beginning of the game. Moreover, the rows and column are numbered starting from 0, **_not_** 1!

2. **Commands**

- _Reveal a cell_ : Enter 'R row column'
- _Flag/Unflag a cell_: Enter 'F row column'
- _Quit the game_: Enter 'Q'

3.  **Symbols**

- '\*' - uncovered cell
- 'F' - flagged cell
- '.' - revealed safe cell with no adjacent mines
- '1-8' - revealed safe cell with the number of adjacent mines
- 'X' - revealed mine (be aware: you lose when you see this!!)

4.  **How to win**

- You need to reveal all the safe cells.
