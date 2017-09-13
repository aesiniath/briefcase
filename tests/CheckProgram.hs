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
    describe "Money type" $ do
        it "Show instance behaves converting fractional amounts to Fixed 2" $ do
            show (money 3) `shouldBe` "3.00"
            show (money 3.1) `shouldBe` "3.10"
            show (money 3.03) `shouldBe` "3.03"
            show (money 3.029) `shouldBe` "3.03"
            show (money (-3.029)) `shouldBe` "-3.03"
            show (money 0) `shouldBe` "0.00"
            show (money 0.1) `shouldBe` "0.10"
            show (money 0.10) `shouldBe` "0.10"
            show (money 0.01) `shouldBe` "0.01"
            show (money 0.001) `shouldBe` "0.00"

    describe "Basic Time Value of Money calculations" $ do
        it "instantaneous future value of an immediate cashflow is itself" $ do
            futureValue rate 0 x `shouldBe` x

        it "future value of an immediate cashflow one year out" $ do
            futureValue rate 1 1000.00 `shouldBe` 1050.00

        it "present value of an immediate cashflow is itself" $ do
            presentValue rate 0 x `shouldBe` x
