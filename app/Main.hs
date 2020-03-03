module Main where

import Lib
--import System.Environment
--import qualified Data.Text as Text
--import qualified Data.Text.IO as Text
--import Data.Char
import Control.Monad

main :: IO ()
main = do
    --content <- Text.readFile "assets/moby-dick.txt" :: IO Text.Text
    --let numbers = fmap ord $ Text.unpack content
    let numbers = [65279,13,10,84,104]
    forever $ do
        a <- readLn :: IO [Int]
        print $ lis a
    print $ numbers
    print $ lis numbers

