module Main where

import Lib
import System.Environment
import System.IO
import System.Environment
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.Char

main :: IO ()
main = do
    fileName:[] <- getArgs
    content <- B.readFile fileName
    putStrLn $ fmap (chr . fromIntegral) $ lis fromIntegral B.foldl' content

