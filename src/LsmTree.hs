{-# LANGUAGE DerivingStrategies #-}

module LsmTree
       ( -- * Data type
         Tree
       , Table
       , FileSystem

         -- * Update
       , emptyTree
       , initTree
       , lookupTree
       , initFileSystem
       , insertTree
       , insertTable
         -- * foldr
       , sizeTree
       ) where

import Data.Foldable (foldl')

-- | Data type that stores values while program is working.
data Tree a
    = TreeNode (Tree a) !a (Tree a)
    | Leaf
    deriving stock (Eq, Show)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (TreeNode l x r) = foldMap f l <> f x <> foldMap f r
    -- ^ preorder traverse by default
    foldr _ ini Leaf = ini
    foldr f ini (TreeNode l x r) = f x (foldr f (foldr f ini r) l)

sizeTree :: Tree a -> Int
sizeTree = foldl' ((succ .) . const) 0

emptyTree :: Tree a
emptyTree = Leaf

initTree :: a -> Tree a
initTree x = TreeNode Leaf x Leaf

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x tree = case tree of
    Leaf             -> initTree x
    TreeNode l val r -> case x < val of
        True  -> TreeNode (insertTree x l) val r
        False -> TreeNode l val (insertTree x r)

lookupTree :: Ord a => a -> Tree a -> Bool
lookupTree _ Leaf = False
lookupTree x (TreeNode l y r) = 
    case compare x y of
        LT -> lookupTree x l
        GT -> lookupTree x r
        EQ -> True


-- | Data type that stores info about file.
data Table = Table
    { fileName :: FilePath
    , size :: Int -- ^ current count of fields in the file
    } deriving stock (Eq, Show)

type FileSystem = [Table]

-- | Upload file's information to FileSystem
initFileSystem :: FilePath -> IO FileSystem
initFileSystem _ = undefined

insertTable :: FilePath -> String -> FileSystem -> IO FileSystem
insertTable name lst fs = do
    writeFile name lst
    return $ Table { fileName = name, size = length lst } : fs
