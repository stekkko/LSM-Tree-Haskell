module LsmTreeSpec (spec) where

import Test.Hspec
import LsmTree ( -- * Data
                 Tree
                 -- * Functions
               , insertTree
               , initTree
               , emptyTree
               , heightTree
               , sizeTree )

tree :: Tree Int
tree = insertTree 1 $ insertTree 3 $ insertTree 5 $
       insertTree 7 $ insertTree 2 $ insertTree 6 $
       initTree 4
--      4
--     / \
--    /   \ 
--   2     6
--  / \   / \
-- 1   3 5   7

spec :: Spec
spec = do
  describe "Testing LsmTree module:" $ do
{-  it "Should be correct testing trees: " $ do
      emptyTree `shouldBe` Leaf
      tree `shouldBe` TreeNode (TreeNode (TreeNode Leaf 1 Leaf)
                            2 (TreeNode Leaf 3 Leaf)) 4 (TreeNode
                            (TreeNode Leaf 5 Leaf) 6 (TreeNode Leaf 7 Leaf))
-}
    it "Should work on empty tree:" $ do
      heightTree emptyTree `shouldBe` (0 :: Int)
      sizeTree   emptyTree `shouldBe` (0 :: Int)
    it "Should work on non-empty tree:" $ do
      heightTree tree `shouldBe` (3 :: Int)
      sizeTree   tree `shouldBe` (7 :: Int)
