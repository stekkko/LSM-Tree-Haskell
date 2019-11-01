{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

module LSM.Tree
       ( -- * Data type
         Tree (Node, Leaf)
         -- * Update
       , empty
       , init
       , lookup
       , insert
         -- * foldr
       , size
       , height
       ) where

import Prelude hiding (lookup, init)
import Data.Foldable (foldl')

-- | Data type that stores values while program is working.
data Tree a
    = Node (Tree a) !a (Tree a)
    | Leaf
    deriving stock (Eq, Show)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r
        -- * inorder traverse by default
    foldr _ ini Leaf = ini
    foldr f ini (Node l x r) = foldr f (f x (foldr f ini r)) l

size :: Tree a -> Int
size = foldl' (\len _ -> len + 1) 0

foldTree
    :: forall b a.
    b -- ^ constant when leaf reached
    -> (b -> a -> b -> b) -- ^ function when node reached
    -> Tree a 
    -> b
foldTree leaf node = \case
    Leaf -> leaf
    (Node l x r) -> node (foldT l) x (foldT r)
  where
    foldT = foldTree leaf node

height :: Tree a -> Int
height = foldTree 0 (\l _ r -> 1 + max l r)
                    
empty :: Tree a
empty = Leaf

init :: a -> Tree a
init x = Node Leaf x Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert x = \case
    Leaf -> init x
    Node l val r -> 
        if x < val
        then Node (insert x l) val r
        else Node l val (insert x r)

lookup :: Ord a => a -> Tree a -> Bool
lookup _ Leaf = False
lookup x (Node l y r) = 
    case compare x y of
        LT -> lookup x l
        GT -> lookup x r
        EQ -> True
