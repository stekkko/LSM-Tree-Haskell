module Main (main) where

import Test.Hspec

import qualified LsmTreeSpec as LsmTree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    LsmTree.spec
