Andrew Anderson, with subsequent modifications by Andrew Butterfield

This project is a simple dungeon-crawling game written in Haskell.

You can download the Haskell platform (compiler and libraries) at http://www.haskell.org/platform/

To build and run the project after cloning the repository,
open a terminal in the project folder and do:

    $ cabal configure && cabal build
    $ ./dist/build/haskell-game/haskell-game

Use the W, A, S, and D keys to move the player. Ctrl-C quits the game.

### Project Structure

- Main.hs: the main function
