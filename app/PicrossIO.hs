module PicrossIO where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import System.IO

readSolution :: FilePath -> IO (M.Matrix Bool)
readSolution path = M.fromLists <$> (map handleLine) <$> lines <$> readFile path
    where handleLine = map handleChar
          handleChar ' ' = False
          handleChar '-' = False
          handleChar 'O' = True

toLists :: M.Matrix a -> [[a]]
toLists m = V.toList <$> (flip M.getRow m) <$> [1..(M.nrows m)]

writeSolution :: FilePath -> M.Matrix Bool -> IO ()
writeSolution path mat = withFile path WriteMode $ hWriteSolution mat

hWriteSolution :: M.Matrix Bool -> Handle -> IO ()
hWriteSolution mat hdl = sequence_ $ (hPutStrLn hdl) <$> (fmap printable) <$> (toLists mat)
    where printable True =  'O'
          printable False = '-'
