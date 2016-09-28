module ForumSpec where

import MyArray
import Ix
import Test.Hspec

spec :: Spec
spec = context "forum tests" $ do
    context "Friday, 11 March 2016, 06:14" $ do
        it "range (1,0)" $
            range (1::Int,0) `shouldBe` []

        it "listArray (1,0) [1..]" $
            listArray (1::Int,0) [1..] `shouldSatisfy` (null . elems)

        it "range ((0,1),(1,0))" $
            range ((0,1),(1::Int,0::Int)) `shouldBe` []

        it "listArray ((0,1),(1,0)) [1..]" $
            listArray ((0,1),(1::Int,0::Int)) [1..] `shouldSatisfy` (null . elems)

    context "Sunday, 13 March 2016, 19:36" $
        it "listArray (1, 5) ['a']" $
            elems (listArray (1, 5::Int) ['a']) `shouldBe` "a"

    context "Sunday, 13 March 2016, 19:37" $
        it "elems $ (array (1,10000) [(1,'a'), (15, 'b')]) // [(100,'c')]" $
            (elems $ (array (1,10000::Int) [(1,'a'), (15, 'b')]) // [(100,'c')]) `shouldBe` "abc"

    context "Sunday, 13 March 2016, 19:40" $
        it "range ((5, 'a'), (7,'b'))" $ do
            range ((5, 'a'), (7::Int,'b')) `shouldBe` [(5,'a'),(5,'b'),(6,'a'),(6,'b'),(7,'a'),(7,'b')]
