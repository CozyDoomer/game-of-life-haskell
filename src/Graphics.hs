module Graphics where

import Lib

import Graphics.Gloss
import Data.Array

squareSize :: Num a => a
squareSize = 25

visualize :: Board -> Picture
visualize b =
    Pictures
    [ Color color sq | (x, y) <- indices boardArray,
        let xv = width + (x * 30)
            yv = height - (y * 30)
            sq = square (fromIntegral xv) (fromIntegral yv)
            color = if boardArray ! (y, x) == "O" then black else white
    ]
    where boardArray = unpack b
          width = fst . snd $ bounds boardArray
          height = snd . snd $ bounds boardArray

square :: Float -> Float -> Picture
square x y = Polygon [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
    where s = squareSize