module Main where

import Lib
import System.Posix.Unistd

{-
 clear screen
 show board
 wait for 0.5 seconds
 recursive call with updated board
-}
gameOfLife :: Board -> IO ()
gameOfLife b =  if not $ isBoardEmpty b then
                    do clearScreen
                       displayBoard b
                       usleep 500000
                       gameOfLife $ updateBoard b
                else
                    do clearScreen
                       putStr "Done.\n"

main :: IO()
main = gameOfLife rpentominoPredecessor
