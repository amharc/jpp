{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module MyArray
    ( module Ix
    , toList
    , fromList
    , listArray
    , (!)
    , (!?)
    , elems
    , array
    , (//)
    , Array
    , update
    ) where

import Ix

data Nat = Zero | Succ Nat

data Color = Black | Red

data Node (c :: Color) (bh :: Nat) k v where
    Leaf :: Node 'Black 'Zero k v
    RNode ::   Node 'Black bh k v
            -> k -> v
            -> Node 'Black bh k v
            -> Node 'Red bh k v
    BNode ::   Node cl bh k v
            -> k -> v
            -> Node cr bh k v
            -> Node 'Black ('Succ bh) k v
deriving instance (Show k, Show v) => Show (Node c bh k v)
deriving instance Functor (Node c bh k)

data Tree k v where
    Tree :: Node 'Black bh k v -> Tree k v
deriving instance (Show k, Show v) => Show (Tree k v)
deriving instance Functor (Tree k)

data Insert c bh k v where
    BOk :: Node 'Black bh k v -> Insert 'Black bh k v
    BRed :: Node 'Red bh k v -> Insert 'Black bh k v

    ROk :: Node 'Red bh k v -> Insert 'Red bh k v
    RLeftRed :: Node 'Red bh k v -> k -> v -> Node 'Black bh k v -> Insert 'Red bh k v
    RRightRed :: Node 'Black bh k v -> k -> v -> Node 'Red bh k v -> Insert 'Red bh k v

insertNode :: Ord k => k -> v -> Node c bh k v -> Insert c bh k v
insertNode k v Leaf = BRed $ RNode Leaf k v Leaf
insertNode ik iv (RNode l k v r)
    | ik < k = case insertNode ik iv l of
                 BOk l' -> ROk $ RNode l' k v r
                 BRed l' -> RLeftRed l' k v r
    | ik > k = case insertNode ik iv r of
                 BOk r' -> ROk $ RNode l k v r'
                 BRed r' -> RRightRed l k v r'
    | otherwise = ROk $ RNode l ik iv r
insertNode ik iv (BNode l k v r)
    | ik < k = case insertNode ik iv l of
                 BOk l' -> BOk $ BNode l' k v r
                 BRed l' -> BOk $ BNode l' k v r
                 ROk l' -> BOk $ BNode l' k v r
                 RLeftRed (RNode lll llk llv llr) lk lv lr -> BRed $
                    RNode (BNode lll llk llv llr) lk lv (BNode lr k v r)
                 RRightRed ll lk lv (RNode lrl lrk lrv lrr) -> BRed $
                    RNode (BNode ll lk lv lrl) lrk lrv (BNode lrr k v r)
    | ik > k = case insertNode ik iv r of
                 BOk r' -> BOk $ BNode l k v r'
                 BRed r' -> BOk $ BNode l k v r'
                 ROk r' -> BOk $ BNode l k v r'
                 RLeftRed (RNode rll rlk rlv rlr) rk rv rr -> BRed $
                    RNode (BNode l k v rll) rlk rlv (BNode rlr rk rv rr)
                 RRightRed rl rk rv (RNode rrl rrk rrv rrr) -> BRed $
                    RNode (BNode l k v rl) rk rv (BNode rrl rrk rrv rrr)
    | otherwise = BOk $ BNode l ik iv r

findNode :: Ord k => k -> Node c bh k v -> Maybe v
findNode _ Leaf = Nothing
findNode ik (RNode l k v r)
    | ik < k = findNode ik l
    | ik == k = Just v
    | otherwise = findNode ik r
findNode ik (BNode l k v r)
    | ik < k = findNode ik l
    | ik == k = Just v
    | otherwise = findNode ik r

insertTree :: Ord k => k -> v -> Tree k v -> Tree k v
insertTree k v (Tree n) = case insertNode k v n of
    BOk n' -> Tree n'
    BRed (RNode l k' v' r) -> Tree $ BNode l k' v' r

findTree :: Ord k => k -> Tree k v -> Maybe v
findTree k (Tree n) = findNode k n

emptyTree :: Tree k v
emptyTree = Tree Leaf

fromList :: Ord k => [(k, v)] -> Tree k v
fromList = foldl (flip $ uncurry insertTree) emptyTree

toList :: Tree k v -> [(k, v)]
toList (Tree n) = go n []
  where
    go :: Node c bh k v -> [(k, v)] -> [(k, v)]
    go Leaf = id
    go (RNode l k v r) = go l . ((k, v) :) . go r
    go (BNode l k v r) = go l . ((k, v) :) . go r

data Array i e = Array
    { bounds :: (i, i)
    , tree :: Tree i e
    }

deriving instance (Show i, Show e) => Show (Array i e)
deriving instance Functor (Array i)

listArray :: Ix i => (i, i) -> [e] -> Array i e
listArray bounds vals = Array{..}
  where
    tree = fromList $ zip (range bounds) vals

(!?) :: Ix i => Array i e -> i -> Maybe e
Array{..} !? i
    | inRange bounds i = findTree i tree
    | otherwise = Nothing

-- Heh, importing Data.Maybe is prohibited :(
{-# ANN module "HLint: ignore Use fromMaybe" #-}
(!) :: Ix i => Array i e -> i -> e
a ! i = case a !? i of
    Just x -> x
    Nothing -> error "Unbound index"

elems :: Ix i => Array i e -> [e]
elems Array{..} = snd <$> toList tree

array :: Ix i => (i, i) -> [(i, e)] -> Array i e
array bounds vals
    | all (inRange bounds . fst) vals =
        Array {bounds = bounds, tree = emptyTree} // vals
    | otherwise = error "out of bounds"

update :: Ix i => i -> e -> Array i e -> Array i e
update k v a = a {tree = insertTree k v $ tree a}

(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) = foldl . flip $ uncurry update
