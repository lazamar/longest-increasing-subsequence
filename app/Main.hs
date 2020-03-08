module Main where

import Lib
import System.Environment
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
    fileName:[] <- getArgs
    content <- readFile fileName
    print $ lis foldl' content

