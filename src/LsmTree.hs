{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DatatypeContexts #-}

module LsmTree
       ( -- * Data type
         Tree
       , Table

         -- * Update
       , emptyTree
       , emptyTable
       , initTree
       , initTable
       , insertTree
       ) where

-- | Data type that stores values while program is working.
data Ord a => Tree a
    = TreeNode (Tree a) a (Tree a)
    | Leaf
    deriving stock (Eq, Show)

emptyTree :: Tree a
emptyTree = Leaf

initTree :: Ord a => a -> Tree a
initTree x = TreeNode Leaf x Leaf

insertTree :: Ord a => Tree a -> a -> Tree a
insertTree Leaf x = initTree x
insertTree (TreeNode l val r) x
   | x < val   = TreeNode (insertTree l x) val r
   | otherwise = TreeNode l val (insertTree r x)

-- | Data type that stores values in memory.
data Ord a => Table a = Table [a]
    deriving stock (Eq, Show)

emptyTable :: Ord a => Table a
emptyTable = Table []

initTable :: Ord a => a -> Table a
initTable x = Table [x]

insertTable :: Ord a => Table a -> a -> Table a
insertTable = undefined
