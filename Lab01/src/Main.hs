module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO (stdin, stdout, hReady, hPutStr, hGetChar, hPutChar, hFlush, hSetBuffering, hSetEcho, Handle(), BufferMode(..))
import qualified System.Console.ANSI as Console

{- Data types -}

data Tile = Grass
          | Wall
          | Empty

data Map = Map Int Int [[Tile]]

type Point = (Int, Int)

data Object = Player Point
            | Chest Point

data Scene = Scene Map Object [Object]

{- Utility functions to do stuff we need to do -}

width :: Map -> Int
width (Map w _ _) = w

height :: Map -> Int
height (Map _ h _) = h

contents :: Map -> [[Tile]]
contents (Map _ _ c) = c

getPlayer :: Scene -> Object
getPlayer (Scene _ p _) = p

getObjects :: Scene -> [Object]
getObjects (Scene _ _ obs) = obs

getPosition :: Object -> Point
getPosition (Player pt) = pt
getPosition (Chest pt) = pt

takeSome :: Int -> [a] -> [a]
takeSome _ []     = []
takeSome 0  _     = []
takeSome n (x:xs) = x : takeSome (n-1) xs

dropSome :: Int -> [a] -> [a]
dropSome _ []     = []
dropSome 0 xs     = xs
dropSome n (x:xs) = dropSome (n-1) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = (takeSome n l):(chunksOf n (dropSome n l))

createMap :: Int -> Int -> String -> Map
createMap w h c =
  Map w h (chunksOf w $ map (read . (:[])) c)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

{- Reading and Showing Tiles and Objects -}

instance Read Tile where
  readsPrec _ "." = [(Grass, "")]
  readsPrec _ "#" = [(Wall, "")]
  readsPrec _ _   = [(Empty, "")]

instance Show Tile where
  show Grass  = "."
  show Wall   = "#"
  show Empty  = " "

instance Show Object where
  show (Player _) = "☃"
  show (Chest _) = "?"

{- Handle a key press from the player -}

handleInput :: Char -> Scene -> Scene
handleInput 'i' (Scene map (Player (x, y)) objects) = (Scene map (Player (x, (y-1))) objects)
handleInput 'j' (Scene map (Player (x, y)) objects) = (Scene map (Player ((x-1), y)) objects)
handleInput 'k' (Scene map (Player (x, y)) objects) = (Scene map (Player (x, (y+1))) objects)
handleInput 'l' (Scene map (Player (x, y)) objects) = (Scene map (Player ((x+1), y)) objects)
handleInput _ x = x

{- Rendering the game world to the console -}

renderMap :: Handle -> Map -> IO ()
renderMap theScreen theMap = do
  let xcoords = [0..((width theMap)-1)]
  let ycoords = [0..((height theMap)-1)]
  let allCoords = concatMap (\ycoord -> zip xcoords $ replicate (width theMap) ycoord) ycoords
  mapM_ (drawItem theMap theScreen) allCoords
  where
    drawItem theMap theScreen (x,y) = do
      let item = (((contents theMap) !! y) !! x)
      Console.hSetCursorPosition theScreen y x
      hPutStr theScreen (show item)

renderObject :: Handle -> Object -> IO ()
renderObject theScreen obj =
  case obj of
    Player (x,y) -> do
      Console.hSetCursorPosition theScreen y x
      hPutStr theScreen (show obj)
    Chest (x,y) -> do
      Console.hSetCursorPosition theScreen y x
      hPutStr theScreen (show obj)

renderScene :: Handle -> Scene -> IO ()
renderScene theScreen (Scene map player objects) = do
  renderMap theScreen map
  mapM_ (renderObject theScreen) objects
  renderObject theScreen player

{- Get input from the player -}

getCharacter :: Handle -> IO Char
getCharacter handle = do
  r <- hReady handle
  if r then
    hGetChar handle
  else return ' '

{- Aliases to make it clearer what's happening in main -}

screen = stdout
keyboard = stdin
frameRate = round $ 1.0e6/24.0

theMap =
  "                    " ++
  "   ###########      " ++
  "  #...........#     " ++
  "  #............#    " ++
  "  #.............#   " ++
  "  #.............#   " ++
  "  #.............#   " ++
  "   #............#   " ++
  "    #...........#   " ++
  "     ###########    " ++
  "                    "

theObjects = [Chest (13,6)]

theScene = Scene (createMap 20 10 theMap)
                 (Player (10,5))
                 theObjects

main = do
  hSetBuffering keyboard NoBuffering
  hSetBuffering screen LineBuffering
  hSetEcho screen False
  Console.hHideCursor screen
  gameScene <- newIORef $ theScene
  forever $ do
    Console.hClearScreen screen
    c <- getCharacter keyboard
    gscene <- readIORef gameScene
    let newScene = handleInput c gscene
    renderScene screen newScene
    Console.hSetCursorPosition screen 2 20

    hPutStr screen (
      "Distance to the chest: " ++
      show
        (distance (getPosition (getPlayer newScene))
                  (getPosition (head (getObjects newScene)))) ++
      " steps.")

    hFlush screen
    writeIORef gameScene newScene
    threadDelay frameRate
