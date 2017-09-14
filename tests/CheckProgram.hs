{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Test.Hspec
import Formatting

import Briefcase.TimeValueOfMoney
import Briefcase.Utilities

main :: IO ()
main = do
    finally
        (hspec suite)
        (putStrLn "")

suite :: Spec
suite =
  let
    x = money 1000.00
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

        it "Formatted output of Money values" $ do
            format (dollarAmount) (money 12475665)    `shouldBe` "$12,475,665"
            format (dollarAmount) (money 12475665.99) `shouldBe` "$12,475,666"

    describe "Basic Time Value of Money calculations" $ do
        it "instantaneous future value of an immediate cashflow is itself" $ do
            futureValue rate 0 x `shouldBe` x

        it "future value of an immediate cashflow one year out" $ do
            futureValue rate 1 1000.00 `shouldBe` 1050.00

        it "present value of an immediate cashflow is itself" $ do
            presentValue rate 0 x `shouldBe` x

        it "present value of an amount one year out" $ do
            presentValue rate 1 1050.00 `shouldBe` 1000.00

        it "present value of an amount two years out" $ do
            presentValue rate 2 1102.50 `shouldBe` 1000.00

    describe "Net present value of cash flows" $ do
        it "present value of an immediate cashflow is itself" $ do
            netPresentValue rate [(x,0)] `shouldBe` x

        it "present value of an single future cashflow" $ do
            netPresentValue rate [(1050.00,1)] `shouldBe` x
            netPresentValue rate [(1102.50,2)] `shouldBe` x

        it "present value of an two future cashflows" $ do
            netPresentValue rate [(1050.00,1), (1102.50,2)] `shouldBe` x * 2

{-
    Example from https://en.wikipedia.org/wiki/Present_value
-}
        it "present value of an example stream" $ do
            netPresentValue rate [(100,1), (-50,2), (35,3)] `shouldBe` 80.12

