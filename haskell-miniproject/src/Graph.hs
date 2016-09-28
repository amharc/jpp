{-# LANGUAGE FlexibleContexts #-}
module Graph(Graph, readGraph, reachableFrom) where

import MyArray
import Control.Monad.State

type Vertex = Int

newtype Graph = Graph (Array Vertex [Vertex])
    deriving Show

readGraph :: [(Vertex, [Vertex])] -> Graph
readGraph [] = error "Empty graph"
readGraph lst = Graph $ array (minimum ixs, maximum ixs) lst
  where
    ixs = map fst lst ++ (lst >>= snd)

reachableFrom :: Vertex -> Graph -> [Vertex]
reachableFrom v0 (Graph g) = snd $ execState (go v0) (fmap (const False) g, [])
  where
    go v = do
        vis <- gets $ (! v) . fst
        unless vis $ do
            modify $ \(arr, res) -> (arr // [(v, True)], v : res)
            mapM_ go $ g ! v
