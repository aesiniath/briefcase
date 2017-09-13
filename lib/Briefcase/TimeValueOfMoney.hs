{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.TimeValueOfMoney (
    Money(..)
  , money
  , FutureValue
  , PresentValue
  , Rate
  , Periods
  , futureValue
  , presentValue
  , netPresentValue
) where

import Data.Fixed
import Data.Foldable (foldl')

{-
    I know perfectly well that Double isn't an appropriate type for actual
    amounts of money. Here we're just doing some basic estimation, so use
    floating point for now.
-}
newtype Money = Amount Double
    deriving (Num, Fractional, Real, RealFrac, Eq, Ord)

--
-- | Take an amount as a Double and round it to hundredths of a dollar, aka
-- cents. Smart constructor (such a silly name) for Money type.
--
money :: Double -> Money
money x =
  let
    cents  = x * 100
    cents' = fromIntegral (round cents)
    value  = cents' / 100
  in
    Amount value

instance Show Money where
    show (Amount x) =
      let
        cents = show (round (x * 100))
        digits = length cents
        (whole,part) = splitAt (digits - 2) cents
        whole' = if length whole == 0 then "0" else whole
        part'  = if length part == 1  then '0':part else part
      in
        whole' ++ "." ++ part'

type PresentValue = Money
type FutureValue = Money

type Rate = Double

type Periods = Int
type Year = Int

--
-- | Future value of an amount, discounted over time. You rarely ever need
-- this, but conceptually it is the foundation of the rest of time value of
-- money.
--
futureValue :: Rate -> Periods -> PresentValue -> FutureValue
futureValue rate periods present =
  let
    r = rate
    n = periods
    pv = realToFrac present
    fv = pv * (1 + r) ^ n
  in
    money fv

presentValue :: Rate -> Periods -> FutureValue -> PresentValue
presentValue rate periods future =
  let
    r = rate
    n = periods
    fv = realToFrac future
    pv = fv / ((1 + r) ^ n)
  in
    money pv

--
-- | Given a stream of cash flows (assumed for now to be annual), calculate the
-- present value of the stream. Assumes a constant discount rate throughout.
--
netPresentValue :: Rate -> [(FutureValue,Year)] -> PresentValue
netPresentValue rate fvs =
  let
    f :: PresentValue -> (FutureValue,Year) -> PresentValue
    f acc (fv,n) = acc + presentValue rate n fv
  in
    foldl' f (money 0.00) fvs

