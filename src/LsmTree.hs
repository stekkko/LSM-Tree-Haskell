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
       ) where

-- | Data type that stores values while program is working.
data Tree a
    = TreeNode (Tree a) a (Tree a)
    | Leaf
    deriving stock (Eq, Show)

emptyTree :: Tree a
emptyTree = Leaf

initTree :: a -> Tree a
initTree x = TreeNode Leaf x Leaf

-- | Data type that stores values in memory.
data Table a = Table [a]
    deriving stock (Eq, Show)

emptyTable :: Table a
emptyTable = Table []

initTable :: a -> Table a
initTable x = Table [x]
