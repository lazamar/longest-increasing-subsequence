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
    let numbers =  take 10 $ drop 490 $ fmap ord $ Text.unpack content
    print numbers
    print $ lis numbers

