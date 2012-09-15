{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import ReadArgs

specs :: Spec
specs = do
  describe "parseArgsFrom" $ do
    it "can parse zero arguments" $ do
      parseArgsFrom [] == Just ()

    it "can parse a single argument" $ do
      parseArgsFrom ["3"] == Just ((3 :: Int) :& ())

    it "can parse a pair of arguments" $ do
      parseArgsFrom ["3", "4"] == Just (3 :: Int, 4 :: Int)

    it "can parse a string without double quotes" $ do
      parseArgsFrom ["abe", "bar"] == Just ("abe", "bar")

    it "can parse a character without single quotes" $ do
      parseArgsFrom ["a", "b"] == Just ('a','b')

    it "can parse a triplet of arguments" $ do
      parseArgsFrom ["3", "steve", "1.0"] == Just (3 :: Int, "steve", 1.0 :: Float)

    it "can parse an optional argument at the end" $ do
      parseArgsFrom ["3", "steve", "1.0"] == Just (3 :: Int, "steve", Just 1.0 :: Maybe Float)
      &&
      parseArgsFrom ["3", "steve"] == Just (3 :: Int, "steve", Nothing :: Maybe Float)

    it "can parse an optional argument in the middle" $ do
      parseArgsFrom ["3", "steve", "1.0"] == Just (3 :: Int, Just "steve", 1.0 :: Float)
      &&
      parseArgsFrom ["3", "1.0"] == Just (3 :: Int, Nothing :: Maybe String, 1.0 :: Float)

    it "can parse an optional argument at the front" $ do
      parseArgsFrom ["3", "steve", "1.0"] == Just (Just 3 :: Maybe Int, "steve", 1.0 :: Float)
      &&
      parseArgsFrom ["steve", "1.0"] == Just (Nothing:: Maybe Int, "steve", 1.0 :: Float)

    it "can parse optional arguments greedily" $ do
      parseArgsFrom ["a", "b"] == Just (Just "a", Just "b", Nothing :: Maybe String)

    it "can parse optional arguments non-greedily" $ do
      parseArgsFrom ["a", "b"] == Just (Just "a", NonGreedy Nothing :: NonGreedy Maybe String, Just "b")
      &&
      parseArgsFrom ["a", "b"] == Just (NonGreedy Nothing :: NonGreedy Maybe String, Just "a", Just "b")

    it "can parse a variable number of arguments at the end" $ do
      parseArgsFrom ["3", "steve"] == Just (3 :: Int, "steve", [] :: [Float])
      &&
      parseArgsFrom ["3", "steve", "1.0"] == Just (3 :: Int, "steve", [1.0] :: [Float])
      &&
      parseArgsFrom ["3", "steve", "1.0", "2.0", "3.0"] == Just (3 :: Int, "steve", [1,2,3] :: [Float])

    it "can parse a variable number of arguments in the middle" $ do
      parseArgsFrom ["3", "1.0"] == Just (3 :: Int, [] :: [String], 1.0 :: Float)
      &&
      parseArgsFrom ["3", "a", "1.0"] == Just (3 :: Int, ["a"], 1.0 :: Float)
      &&
      parseArgsFrom ["3", "a", "b", "c", "1.0"] == Just (3 :: Int, ["a","b","c"], 1.0 :: Float)

    it "can parse a variable number of arguments at the front" $ do
      parseArgsFrom ["steve", "1.0"] == Just ([] :: [Int], "steve", 1.0 :: Float)
      &&
      parseArgsFrom ["1", "steve", "1.0"] == Just ([1] :: [Int], "steve", 1.0 :: Float)
      &&
      parseArgsFrom ["1", "2", "3", "steve", "1.0"] == Just ([1,2,3] :: [Int], "steve", 1.0 :: Float)

    it "can parse variable arguments greedily" $ do
      parseArgsFrom ["1", "2"] == Just ([1,2] :: [Int], [] :: [Int], [] :: [Int])

    it "can parse variable arguments non-greedily" $ do
      parseArgsFrom ["1", "2"] == Just (NonGreedy [] :: NonGreedy [] Int, [1,2] :: [Int], [] :: [Int])
      &&
      parseArgsFrom ["1", "2"] == Just (NonGreedy [] :: NonGreedy [] Int, NonGreedy [] :: NonGreedy [] Int, [1,2] :: [Int])

    it "can parse adjacent sets of variable arguments" $ do
      parseArgsFrom ["1", "2", "a", "b"] == Just ([1,2] :: [Int], ["a","b"] :: [String])

    it "can parse a single argument without tuples" $ do
      parseArgsFrom ["3"] == Just (3 :: Int)

    it "can parse an optional argument without tuples" $ do
      parseArgsFrom ["3"] == Just (Just 3 :: Maybe Int)
      &&
      parseArgsFrom [] == Just (Nothing :: Maybe Int)

    it "can parse a variable argument without tuples" $ do
      parseArgsFrom ["1","2","3"] == Just ([1,2,3] :: [Int])
      &&
      parseArgsFrom [] == Just ([] :: [Int])

main :: IO ()
main = hspec specs
