{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module IxSpec where

import qualified Data.Ix as DI
import Data.Proxy
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Ix

data RangeAndIndex a = RangeAndIndex
    { raiRange :: (a, a)
    , raiIndex :: a
    }
    deriving Show

instance (Arbitrary a, Show a, DI.Ix a) => Arbitrary (RangeAndIndex a) where
    arbitrary = do
        rg <- arbitrary `suchThat` (not . null . DI.range)
        ix <- elements $ DI.range rg
        pure $ RangeAndIndex rg ix

testIx :: _ => _ a -> Spec
testIx (_ :: _ a) = do
    prop "range" $ \(r :: (a, a)) -> range r == DI.range r
    prop "inRange" $ \r (i :: a) -> inRange r i == DI.inRange r i
    prop "rangeSize" $ \(r :: (a, a)) -> rangeSize r == DI.rangeSize r
    prop "index" $ \(RangeAndIndex r i :: RangeAndIndex a) -> index r i == DI.index r i

spec :: Spec
spec = context "tree" $ do
    context "textIx" $ do
        context "Int" $ testIx (Proxy :: Proxy Int)
        context "Char" $ testIx (Proxy :: Proxy Char)
        context "Integer" $ testIx (Proxy :: Proxy Integer)
        context "Integer, Char" $ testIx (Proxy :: Proxy (Integer, Char))
        context "Integer, Integer" $ testIx (Proxy :: Proxy (Integer, Integer))
        context "Int, Int" $ testIx (Proxy :: Proxy (Int, Int))
        context "(Char, Char), Char" $ testIx (Proxy :: Proxy ((Char, Char), Char))

    context "Integer" $
        context "huge numbers" $ do
            it "range" $ range (n, n + 7) `shouldBe` [n..n + 7]
            it "inRange T" $ inRange (n, n + 7) (n + 3) `shouldBe` True
            it "inRange F" $ inRange (n, n + 7) (n + 8) `shouldBe` False
            it "rangeSIze" $ rangeSize (n, n + 7) `shouldBe` 8
          where
            n :: Integer
            n = 100000000000000000000000000000000000000000000000000000000000000000000000000000000000
