{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.Hourglass
import Test.Hspec
import Formatting

import Briefcase.CashFlow
import Briefcase.Money
import Briefcase.TimeValue
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
            format (dollarAmount) (money (-9.99)) `shouldBe` "-$10"

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

    describe "Date ranges" $ do
        it "generates quarterly increments" $ 
          let
            seed = Date 2019 April 08
          in do
            take 4 (quarterlyDates seed) `shouldBe`
                [ Date 2019 April 08
                , Date 2019 July 08
                , Date 2019 October 08
                , Date 2020 January 08
                ]

        it "generates monthly increments" $ 
          let
            seed = Date 2019 January 01
          in do
            take 12 (monthlyDates seed) `shouldBe`
                [ Date 2019 January 01
                , Date 2019 February 01
                , Date 2019 March 01
                , Date 2019 April 01
                , Date 2019 May 01
                , Date 2019 June 01
                , Date 2019 July 01
                , Date 2019 August 01
                , Date 2019 September 01
                , Date 2019 October 01
                , Date 2019 November 01
                , Date 2019 December 01
                ]
{-
                [ Date 2019 January 31
                , Date 2019 February 28
                , Date 2019 March 31
                , Date 2019 April 30
                , Date 2019 May 31
                , Date 2019 June 30
                , Date 2019 July 31
                , Date 2019 August 31
                , Date 2019 September 30
                , Date 2019 October 31
                , Date 2019 November 30
                , Date 2019 December 31
                ]
-}
        it "generates fortnightly increments" $ 
          let
            seed = Date 2019 April 10
          in do
            take 3 (fortnightlyDates seed) `shouldBe`
                [ Date 2019 April 10
                , Date 2019 April 24
                , Date 2019 May 08
                ]

        it "extracts date ranges" $ 
          let
            list = take 5 (monthlyDates (Date 2019 January 01))
          in do
            rangeDates (Date 2019 January 15) (Date 2019 March 15) list `shouldBe`
                [ Date 2019 February 01
                , Date 2019 March 01
                ]
            rangeDates (Date 2019 February 01) (Date 2019 March 31) list `shouldBe`
                [ Date 2019 February 01
                , Date 2019 March 01
                ]
            rangeDates (Date 2019 February 01) (Date 2019 April 01) list `shouldBe`
                [ Date 2019 February 01
                , Date 2019 March 01
                , Date 2019 April 01
                ]
            rangeDates (Date 2019 January 01) (Date 2019 May 01) list `shouldBe`
                [ Date 2019 January 01
                , Date 2019 February 01
                , Date 2019 March 01
                , Date 2019 April 01
                , Date 2019 May 01
                ]