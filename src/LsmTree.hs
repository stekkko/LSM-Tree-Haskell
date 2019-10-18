{-# LANGUAGE DerivingStrategies #-}

module LsmTree
       ( -- * Data type
         Tree
       , Table
       , FileSystem

         -- * Update
       , emptyTree
       , initTree
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

-- | Data type that stores values in memory.
data Table = Table
    { fileName :: String
    , size :: Int
    } deriving stock (Eq, Show)

type FileSystem = [Table]

-- | Upload file's information to FileSystem
initFileSystem :: FilePath -> IO ()
initFileSystem fp = undefined -- ^ TODO
    

insertTable :: String -> [a] -> FileSystem -> IO FileSystem
insertTable name lst fs = do
    writeFile name lst
    return $ Table { fileName = name, size = length lst } : fs
