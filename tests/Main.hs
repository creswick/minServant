module Main where

import           Test.Tasty                ( TestTree
                                           , defaultIngredients
                                           , defaultMainWithIngredients
                                           , testGroup
                                           )
import           Test.Tasty.Ingredients    ( Ingredient)
import           Test.Tasty.Runners.AntXML ( antXMLRunner )

ingredients :: [Ingredient]
ingredients = antXMLRunner : defaultIngredients

main :: IO ()
main = do
  defaultMainWithIngredients ingredients tests

tests :: TestTree
tests = testGroup "Tests"
  [
  ]

