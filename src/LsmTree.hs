{-# LANGUAGE DerivingStrategies #-}

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
data Tree a
    = TreeNode (Tree a) a (Tree a)
    | Leaf
    deriving stock (Eq, Show)

emptyTree :: Tree a
emptyTree = Leaf

initTree :: Ord a => a -> Tree a
initTree x = TreeNode Leaf x Leaf

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x tree = case tree of
    Leaf             -> initTree x
    TreeNode l val r -> case x < val of
        True  -> TreeNode (insertTree x l) val r
        False -> TreeNode l val (insertTree x r)

-- | Data type that stores values in memory.
data Table a = Table [a]
    deriving stock (Eq, Show)

emptyTable :: Ord a => Table a
emptyTable = Table []

initTable :: Ord a => a -> Table a
initTable x = Table [x]

insertTable :: Ord a => Table a -> a -> Table a
insertTable = undefined
