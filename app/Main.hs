module Main where

import Lib
import Control.Concurrent

import Data.Array

{-
 clear screen
 show board
 wait for 0.5 seconds
 recursive call with updated board
-}
gameOfLife :: Board -> IO ()
gameOfLife b = if not $ isBoardEmpty b then
                  do clearScreen
                     displayBoard b
                     threadDelay 500000
                     gameOfLife $ updateBoard b
               else
                  do clearScreen
                     putStr "Done.\n"

main :: IO()
main = do board <- createBoard "examples/blinker_block.txt"
          gameOfLife board