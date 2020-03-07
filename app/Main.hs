module Main where

import Lib
--import System.Environment
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Char
import Data.List
import Control.Monad
import System.IO

main :: IO ()
main = do
    content <- Text.readFile "assets/small.txt"
    print $ lis Text.foldl' content

