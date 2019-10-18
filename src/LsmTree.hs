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

lookupTree :: Ord a => a -> Tree a -> Bool
lookupTree _ Leaf = False
lookupTree x (TreeNode l y r)
    | x > y  = lookupTree x r
    | x < y  = lookupTree x l
    | x == y = True

-- | Data type that stores values in memory.
data Table = Table
    { fileName :: FilePath
    , size :: Int --size of a file
    } deriving stock (Eq, Show)

type FileSystem = [Table]

-- | Upload file's information to FileSystem
initFileSystem :: FilePath -> IO ()
initFileSystem fp = undefined -- ^ TODO

insertTable :: FilePath -> String -> FileSystem -> IO FileSystem
insertTable name lst fs = do
    writeFile name lst
    return $ Table { fileName = name, size = length lst } : fs
