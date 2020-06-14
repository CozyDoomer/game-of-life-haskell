module Lib where

import Data.Array

width :: Int
width = 20

height :: Int
height = 20

-- Board type using Data.Array with custom Show typeclass
newtype Board = B (Array (Int, Int) String)

instance Show Board where
    show (B b) = showRows b 0

showRows :: Array (Int, Int) String -> Int -> String
showRows r i
                                          -- tail recursion
    | i < maxRow = "|" ++ rowStr ++ "|\n" ++ showRows r (i+1)
    |  otherwise = "|" ++ rowStr ++ "|\n"
    where maxRow = fst . snd $ bounds r
          maxCol = snd . snd $ bounds r
          rowStr = concat [ r ! (i, j) ++ " " | j <- [0 .. maxCol] ]

unpack :: Board -> Array (Int, Int) String
unpack (B b) = b

-- example boards
rpentominoPredecessor :: Board
rpentominoPredecessor = B (listArray ((0, 0), (width-1, height-1)) [ if i `elem` alive then "O" else " " | i <- [0..width*height] ])
    where alive = [207, 208, 209, 191, 211, 212]

blinkerAndBlock :: Board
blinkerAndBlock = B (listArray ((0, 0), (width-1, height-1)) [ if i `elem` alive then "O" else " " | i <- [0..width*height] ])
    where alive = [204, 205, 206, 232, 233, 252, 253]

-- command line related
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

displayBoard :: Board -> IO ()
displayBoard b = putStr $ show b

-- count living neighbors; when out of bounds continue on the other side of the board
countNeighbors :: (Int, Int) -> Board -> Int
countNeighbors p b = length . filter (== "O") . map (\i -> boardArray ! i) $ map handleOverflow neighborIndices
    where boardArray = unpack b
          neighborIndices = [ (i, j) | i <- [fst p + 1, fst p, fst p - 1],
                                       j <- [snd p + 1, snd p, snd p - 1],
                                       i /= fst p || j /= snd p]

handleOverflow :: (Int, Int) -> (Int, Int)
handleOverflow (i,j) = (i `mod` width,
                        j `mod` height)

-- game of life logic for each cell: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules
nextCellValue :: (Int, Int) -> Board -> String
nextCellValue (i,j) b
    | boardArray ! (i,j) == "O" && (neighborCount > 3 || neighborCount < 2) = " "
    | boardArray ! (i,j) == " " && neighborCount == 3 = "O"
    | otherwise = boardArray ! (i,j)
    where boardArray = unpack b
          neighborCount = countNeighbors (i, j) b

-- 
updateBoard :: Board -> Board
updateBoard b = B (boardArray // [(i, nextCellValue i b) | i <- indices boardArray])
    where boardArray = unpack b

isBoardEmpty :: Board -> Bool
isBoardEmpty b = not $ or [value == "O" | value <- elems boardArray]
    where boardArray = unpack b
