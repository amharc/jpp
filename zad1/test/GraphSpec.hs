module GraphSpec where

import Graph
import Test.Hspec

sampleGraph = readGraph
    [(1, [2, 3, 8, 10])
    ,(2, [1, 3, 4])
    ,(4, [])
    ,(3, [])
    ,(5, [1])
    ,(6, [])
    ,(7, [7, 10])
    ,(8, [8])
    ,(9, [1])
    ,(10, [9])
    ]

spec :: Spec
spec = context "reachableFrom" $ do
    it "computes vertices reachable from 1 correctly" $
        reachableFrom 1 sampleGraph `shouldMatchList` [1, 2, 3, 4, 8, 9, 10]

    it "computes vertices reachable from 5 correctly" $
        reachableFrom 5 sampleGraph `shouldMatchList` [1, 2, 3, 4, 5, 8, 9, 10]
