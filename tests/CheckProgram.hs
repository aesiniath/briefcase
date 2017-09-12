{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import Briefcase.TimeValueOfMoney

main :: IO ()
main = do
    hspec suite
    putStrLn ""

suite :: Spec
suite =
  let
    x = 1000.00
    rate = 0.05
  in do
    describe "Time Value of Money calculations" $ do
        it "instantaneous future value of an immediate cashflow is itself" $ do
            futureValue rate 0 x `shouldBe` x

        it "future value of an immediate cashflow one year out" $ do
            futureValue rate 1 1000.00 `shouldBe` 1050.00

        it "present value of an immediate cashflow is itself" $ do
            presentValue rate 0 x `shouldBe` x
