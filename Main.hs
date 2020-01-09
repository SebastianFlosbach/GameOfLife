module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Position = (Int, Int)
type Game = [Position]

windowTitle :: String
windowTitle = "Game Of Life"

gameWidth, gameHeight, cellSize :: Int
gameWidth = 15
gameHeight = 10
cellSize = 10

window :: Display
window = InWindow windowTitle (gameWidth * cellSize, gameHeight * cellSize) (10, 10)

background :: Color
background = black

wrapPosition :: Position -> Position
wrapPosition (x,y) = (wrapX x, wrapY y)
 where {
  wrapX x 
   | x < 0 = gameWidth + x 
   | x >= gameWidth = x - gameWidth
   | otherwise = x;
  wrapY y 
   | y < 0 = gameHeight + y
   | y >= gameHeight = y - gameHeight
   | otherwise = y
  }
 

getNeighbours :: Position -> Game -> [Position]
getNeighbours (x,y) game = [pos | pos <- game,
 pos == wrapPosition(x + 1,y) ||
 pos == wrapPosition(x - 1,y) ||
 pos == wrapPosition(x,y + 1) ||
 pos == wrapPosition(x,y - 1) ||
 pos == wrapPosition(x + 1,y + 1) ||
 pos == wrapPosition(x - 1,y - 1) ||
 pos == wrapPosition(x + 1,y - 1) ||
 pos == wrapPosition(x - 1,y + 1)
 ]
 
countNeighbours :: Position -> Game -> Int
countNeighbours pos game = length (getNeighbours pos game)

isAlive :: Position -> Game -> Bool
isAlive pos game = elem pos game

isAliveNextTurn :: Position -> Game -> Bool
isAliveNextTurn pos game
 | neighbourCount == 3 = True
 | isAlive pos game && (neighbourCount == 2) = True
 | otherwise = False
 where neighbourCount = countNeighbours pos game

updateGame :: ViewPort -> Float -> Game -> Game
updateGame _ _ game = [(x,y) | x <- [0..gameWidth], y <- [0..gameHeight], isAliveNextTurn (x,y) game]

renderGame :: Game -> Picture
renderGame game = pictures [renderCell pos | pos <- game]
 where renderCell :: Position -> Picture
       renderCell (x,y) = translate (fromIntegral (x * cellSize)) (fromIntegral (y * cellSize)) $ color green $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)

initBlinker :: Game
initBlinker = [(1,0),(1,1),(1,2)]

initGlider :: Game
initGlider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

main :: IO ()
main = simulate window background 10 initGlider renderGame updateGame
