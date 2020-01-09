module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Position = (Int, Int)
type Coordinates = (Float, Float)
type Game = [Position]

windowTitle :: String
windowTitle = "Game Of Life"

gameWidth, gameHeight, cellSize :: Int
gameWidth = 20
gameHeight = 15
cellSize = 10

window :: Display
window = InWindow windowTitle (gameWidth * cellSize * 2, gameHeight * cellSize * 2) (10, 10)

background :: Color
background = black

toWorldCoords :: Position -> Coordinates
toWorldCoords (x,y) = (fromIntegral(x * cellSize), fromIntegral(y * cellSize))

translate' :: Coordinates -> Picture -> Picture
translate' (x,y) picture = translate x y picture

wrapPosition :: Position -> Position
wrapPosition (x,y) = (wrapX x, wrapY y)
 where {
  wrapX x 
   | x < 0 = gameWidth - 1
   | x >= gameWidth = 0
   | otherwise = x;
  wrapY y 
   | y < 0 = gameHeight - 1
   | y >= gameHeight = 0
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
updateGame _ _ game = [(x,y) | x <- [0..(gameWidth-1)], y <- [0..(gameHeight-1)], isAliveNextTurn (x,y) game]

renderGame :: Game -> Picture
renderGame game = pictures (renderBox : [renderCell pos | pos <- game])
 where renderCell :: Position -> Picture
       renderCell (x,y) = translate' (toWorldCoords (x,y)) $ color green $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)

initStatic :: Game
initStatic = [(0,0),(0,1),(1,0),(1,1)]

initBlinker :: Game
initBlinker = [(0,0),(0,1),(0,2)]

initGlider :: Game
initGlider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

initGame :: Game
initGame = combineGames [initBlinker, initStatic, initGlider]

combineGames :: [Game] -> Game
combineGames [x] = x
combineGames (x:y:xs) = combineGames ([x ++ [pos | pos <- y, not $ elem pos x]] ++ xs)

moveOrigin :: Coordinates -> Coordinates
moveOrigin (x,y) = (x / 2 - ((fromIntegral cellSize) / 2), y / 2 - ((fromIntegral cellSize) / 2))

renderBox :: Picture
renderBox = translate' (moveOrigin $ toWorldCoords(gameWidth, gameHeight)) $ color white $ rectangleWire (fromIntegral(gameWidth * cellSize)) (fromIntegral(gameHeight * cellSize))

main :: IO ()
main = simulate window background 10 initGame renderGame updateGame
