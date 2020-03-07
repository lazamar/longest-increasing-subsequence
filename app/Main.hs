module Main where

import Lib
--import System.Environment
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Char
import Data.List
import Control.Monad
import System.IO
import System.Environment

main :: IO ()
main = do
    fileName:[] <- getArgs
    content <- readFile fileName
    print
        $ fmap (chr . fromIntegral)
        $ lis
        $ fmap (fromIntegral . ord) content

