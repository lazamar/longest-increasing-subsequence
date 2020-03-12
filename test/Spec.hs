import Test.Hspec
import Test.QuickCheck
import Lib
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Data.List
import Data.Word
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Data.Char

toB :: [Word16] -> ByteString
toB = pack . fmap chr . fmap fromIntegral

toW :: [Int] -> [Word16]
toW = fmap fromIntegral

main :: IO ()
main = hspec $ do
    modifyMaxSize (const 100) $ describe "Longest Increasing Subsequence" $ do
        it "is idempotent" $ do
            property $ \as -> lis (toB as) == (lis $ toB . fmap fromIntegral $ lis $ toB as)

        it "Finds trivial results" $ do
            lis (toB [10, 5, 6, 1, 7]) `shouldBe` [5,6,7]

        it "Handles correctly values that displace ending max" $ do
             lis (toB [32,69,110,103,108,105,115,104,13,10])
             `shouldBe`
             [32,69,103,105,115]

        it "Finds result in increasing subsequences" $ do
                let list = take 10 [1..]
                lis (toB list) `shouldBe` nub (fmap fromIntegral list)

        it "Finds result in decreasing subsequences" $ do
                let list = reverse $ take 10 [1..]
                length (lis $ toB list) `shouldBe` 1

    -- modifyMaxSize (const 100) $ describe "Longest Common Subsequence" $ do
    --      it "is idempotent on the left" $ do
    --          property $ \as bs -> lcs as (lcs as bs) == (lcs as bs :: [Word8])

    --      it "is idempotent on the right" $ do
    --          property $ \as bs -> lcs (lcs as bs) bs == (lcs as bs :: [Word8])

    --      it "returns something smaller" $ do
    --          property $ \as bs -> min (length as) (length bs) >= length (lcs as bs :: [Word8])
