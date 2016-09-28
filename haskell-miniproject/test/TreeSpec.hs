module TreeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import MyArray
import qualified Data.Map as M

spec :: Spec
spec = context "tree" $
    prop "fromList and toList are inverses" $ \x -> toList (fromList x) == M.toList (M.fromList (x :: [(Int, Int)]))
