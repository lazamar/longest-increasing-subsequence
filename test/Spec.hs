import Test.Hspec
import Test.QuickCheck
import Lib
import Test.Hspec.Core.QuickCheck (modifyMaxSize)

main :: IO ()
main = hspec $ do
    modifyMaxSize (const 10000) $ describe "Longest Increasing Subsequence" $ do
        it "is idempotent" $ do
            property $ \as -> lis as == (lis $ lis as :: [Int])

    modifyMaxSize (const 10000) $ describe "Longest Common Subsequence" $ do
        it "is idempotent on the left" $ do
            property $ \as bs -> lcs as (lcs as bs) == (lcs as bs :: [Int])

        it "is idempotent on the right" $ do
            property $ \as bs -> lcs (lcs as bs) bs == (lcs as bs :: [Int])

        it "returns something smaller" $ do
            property $ \as bs -> min (length as) (length bs) >= length (lcs as bs :: [Int])
