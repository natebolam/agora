module Main where

import Test.Hspec (hspec)

import qualified Spec

main :: IO ()
main = hspec Spec.spec
