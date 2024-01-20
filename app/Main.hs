module Main where

import Lib
import Graphics

import Control.Concurrent
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color()

{-
 ASCII version (laggy)
 --------------
 clear screen
 show board in terminal
 wait for 0.25 seconds
 recursive call with updated board
-}
asciiGoL :: Board -> IO ()
asciiGoL b
    | not $ isBoardEmpty b = 
        do clearScreen
           displayBoard b
           threadDelay 250000
           asciiGoL $ updateBoard b
    | otherwise = 
        do clearScreen
           putStr "Done.\n"

{-
 graphic version
 ---------------
 basically let `gloss` deal with it :)

 `visualize` creates a square (type `Picture`) for each cell
 this is passed to `simulate` 
   creates a window
   displays the boardstate
   updates 4 times per second by calling `updateBoard`
-}

bgColor :: Color
bgColor = makeColor 255 255 255 255

window :: Display
window = InWindow "Game of Life" (1920, 1080) (0, 0)

graphicGoL :: Board -> IO ()
graphicGoL board = simulate window bgColor 4 board visualize (\_ _ -> updateBoard)

main :: IO()
main = do board <- createBoard "examples/rpentomino_predecessor_big.txt"
          -- putStrLn $ show board
          -- asciiGoL board
          graphicGoL board