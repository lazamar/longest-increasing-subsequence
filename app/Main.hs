module Main where

import Data.Function
import Data.List
import Data.Maybe
import System.Environment
import qualified Data.Set as Set
import Debug.Trace
import System.IO.Unsafe

-- Longest increasing subsequence
lis :: (Show a, Ord a) => [a] -> [a]
lis = buildResult . view . scanr takeMax (Set.empty, Nothing, Nothing) . reverse
    where
        takeMax value (endings, _, preMax) =
            let
                upperBound = fromMaybe value $ Set.lookupGE value endings
                newMax     = fromMaybe value $ Set.lookupMax newEndings
                newPreMax  = Set.lookupLT newMax newEndings
                newEndings = endings
                    & Set.delete upperBound
                    & Set.insert value
            in
            ( newEndings
            , Just newMax
            , if upperBound == value then newPreMax else preMax
            )

        buildResult ((_, max, premax):vals) = takeResult $ foldr f (max:[], premax) (reverse vals)
            where
                f (_, value, valueNext) (subsequence, next) =
                    if value == next
                       then (value:subsequence, valueNext)
                       else (subsequence, next)

                takeResult = catMaybes . fst

lcs :: (Show a, Ord a) => [a] -> [a] -> [a]
lcs l1 l2 =
    [ (idx, x)
      | y <- l1                 -- For all elements of l1
      , (idx, x) <- indexedL2   -- Get all elements of l2
      , x == y                  -- Where their values are the same
    ]
    & view
    & wrapped lis
    & fmap snd
    where
        indexedL2 = zip [1..] l2
        wrapped f = fmap unwrap . f . fmap Wrap

view v = unsafePerformIO $ do
    putStrLn $ unlines $ fmap show v
    return v

newtype Wrap a = Wrap { unwrap :: (Int, a) }
    deriving (Show)

instance Ord (Wrap a) where
    compare a b = compare
            (fst $ unwrap a)
            (fst $ unwrap b)

instance Eq (Wrap a) where
    a == b = fst (unwrap a) == fst (unwrap b)


main :: IO ()
main = do
    one:two:[] <- getArgs
    print $ lcs one two

