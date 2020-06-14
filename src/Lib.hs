module Lib where

import Data.Array

-- Board type using Data.Array with custom Show typeclass
newtype Board = B (Array (Int, Int) String)

instance Show Board where
    show (B b) = showRows b 0

unpack :: Board -> Array (Int, Int) String
unpack (B b) = b

showRows :: Array (Int, Int) String -> Int -> String
showRows r i                              -- tail recursion
    | i < maxRow = "|" ++ rowStr ++ "|\n" ++ showRows r (i+1)
    |  otherwise = "|" ++ rowStr ++ "|\n"
    where maxRow = fst . snd $ bounds r
          maxCol = snd . snd $ bounds r
          rowStr = concat [ r ! (i, j) ++ " " | j <- [0 .. maxCol] ]

createBoard :: String -> IO Board
createBoard s = do fileString <- readFile s
                   -- TODO: read line wise
                   let height = (length $ filter (== '\n') fileString) + 1
                   let fileList = fmap (\x -> [x]) (filter (/= '\n') fileString)
                   let width = length fileList `div` height
                   pure $ B (listArray ((0, 0), (width-1, height-1)) fileList)

-- command line related
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

displayBoard :: Board -> IO ()
displayBoard b = putStr $ show b

-- count living neighbors; when out of bounds continue on the other side of the board
countNeighbors :: (Int, Int) -> Board -> Int
countNeighbors p b = length . filter (== "O") . map (\i -> boardArray ! i) $ map (handleOverflow (width, height)) neighborIndices
    where boardArray = unpack b
          width = fst . snd $ bounds boardArray
          height = snd . snd $ bounds boardArray
          neighborIndices = [ (i, j) | i <- [fst p + 1, fst p, fst p - 1],
                                       j <- [snd p + 1, snd p, snd p - 1],
                                       i /= fst p || j /= snd p]

handleOverflow :: (Int, Int) -> (Int, Int) -> (Int, Int)
handleOverflow (width, height) (i,j) = (i `mod` width,
                                        j `mod` height)

-- game of life logic for each cell: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules
nextCellValue :: (Int, Int) -> Board -> String
nextCellValue (i,j) b
    | boardArray ! (i,j) == "O" && (neighborCount > 3 || neighborCount < 2) = " "
    | boardArray ! (i,j) == " " && neighborCount == 3 = "O"
    | otherwise = boardArray ! (i,j)
    where boardArray = unpack b
          neighborCount = countNeighbors (i, j) b

updateBoard :: Board -> Board
updateBoard b = B (boardArray // [(i, nextCellValue i b) | i <- indices boardArray])
    where boardArray = unpack b

isBoardEmpty :: Board -> Bool
isBoardEmpty b = not $ or [value == "O" | value <- elems boardArray]
    where boardArray = unpack b
