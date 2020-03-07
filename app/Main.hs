module Main where

import Lib
--import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char
import Control.Monad

main :: IO ()
main = do
    content <- Text.readFile "assets/moby-dick.txt" :: IO Text.Text
    let numbers = [5,6,7,1,2,3,4]
    print numbers
    print $ lis numbers

