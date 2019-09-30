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
    deriving(Eq, Show)

emptyTree :: Tree ()
emptyTree = Leaf

initTree :: a -> Tree a
initTree x = TreeNode x

-- | Data type that stores values in memory.
data Table a = Table [a]
    deriving(Eq, Show)

emptyTable :: Table ()
emptyTable = []

initTable a -> Table [a]
initTable x = Table [x]
