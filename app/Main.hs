module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    one:two:[] <- getArgs
    print $ lcs one two

