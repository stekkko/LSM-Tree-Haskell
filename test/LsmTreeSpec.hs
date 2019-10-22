module LsmTreeSpec (spec) where

import Test.Hspec
import qualified LSM.Tree as LSMT
    ( -- * Data
      Tree
      -- * Functions
    , insert
    , init
    , empty
    , height
    , size )

tree :: LSMT.Tree Int
tree = LSMT.insert 1 $ LSMT.insert 3 $ LSMT.insert 5 $
       LSMT.insert 7 $ LSMT.insert 2 $ LSMT.insert 6 $
       LSMT.init 4
--      4
--     / \
--    /   \ 
--   2     6
--  / \   / \
-- 1   3 5   7

spec :: Spec
spec = do
  describe "Testing LSM.Tree module:" $ do
{-  it "Should be correct testing trees: " $ do
      emptyTree `shouldBe` Leaf
      tree `shouldBe` TreeNode (TreeNode (TreeNode Leaf 1 Leaf)
                            2 (TreeNode Leaf 3 Leaf)) 4 (TreeNode
                            (TreeNode Leaf 5 Leaf) 6 (TreeNode Leaf 7 Leaf))
-}
    it "Should work on empty tree:" $ do
      LSMT.height LSMT.empty `shouldBe` (0 :: Int)
      LSMT.size   LSMT.empty `shouldBe` (0 :: Int)
    it "Should work on non-empty tree:" $ do
      LSMT.height tree `shouldBe` (3 :: Int)
      LSMT.size   tree `shouldBe` (7 :: Int)
