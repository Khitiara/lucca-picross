module PicrossGenerator (PicrossHints, rows, cols, hints) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.List

data PicrossHints = Hints { rows :: [[Int]], cols :: [[Int]] }
                  deriving (Show, Eq)

runLengths :: Eq a => V.Vector a -> [(a, Int)]
runLengths = map (\x -> (head x, length x)) . group . V.toList

hint :: V.Vector Bool -> [Int]
hint  = checkEmpty . (map snd) . (filter fst) . runLengths
      where checkEmpty [] = [0]
            checkEmpty xs = xs

rowHint :: Int -> M.Matrix Bool -> [Int]
rowHint = ((.).(.)) hint M.getRow

colHint :: Int -> M.Matrix Bool -> [Int]
colHint = ((.).(.)) hint M.getCol

hints :: M.Matrix Bool -> PicrossHints
hints m = Hints rowHints colHints
    where rowHints = (flip rowHint $ m) <$> [1..(M.nrows m)]
          colHints = (flip colHint $ m) <$> [1..(M.ncols m)]
