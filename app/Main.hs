module Main where

import qualified Data.Matrix as M
import PicrossGenerator
import System.IO (stdout)
import PicrossIO

main :: IO ()
main = printPuzzle puzzle >> (putStrLn $ show $ hints puzzle)

printPuzzle :: M.Matrix Bool -> IO ()
printPuzzle = flip hWriteSolution stdout

puzzle = M.fromLists [[False, True,  True,  True,  True,  False, False, False],
                      [False, True,  True,  False, False, True,  True,  False],
                      [False, True,  True,  True,  True,  False, False, False],
                      [False, True,  True,  False, True,  False, False, True ]]
