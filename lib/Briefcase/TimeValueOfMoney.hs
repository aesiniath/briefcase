{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.TimeValueOfMoney (
    Money( )
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
    This constructor is not exposed so that we can enforce appropriate rounding 
    behaviour; otherwise Amount 5.119 gets you "5.11" not "5.12"
-}

--
-- | A type representing a decimal currency that has fractional units in
-- hundredths. The constructor is /not/ exposed to avoid an unpleasant
-- behaviour in the fixed point being used to represent the value internally.
-- Use the 'money' function below to construct a Money value from an
-- arbitrary integer or decimal.
--

newtype Money = Amount (Fixed E2)
    deriving (Num, Fractional, Real, RealFrac, Eq, Ord)

--
-- | Take an amount as an Int or Double and round it to hundredths of a dollar,
-- aka cents. Smart constructor (such a silly name) for Money type.
--
-- >>> money 5
-- 5.00
--
-- >>> money 5.0
-- 5.00
--
-- >>> money 5.02
-- 5.02
--
-- >>> money 5.029
-- 5.03
--
money :: Double -> Money
money x =
  let
    cents  = x * 100
    cents' = round cents :: Int
    value  = (fromIntegral cents') / 100
  in
    Amount value

{-
    Relies on the fact that the value in Fixed E2 is already truncated to two
    digits, so we don't need to do anything fancy any more; the Show instance
    there is reliable.
-}
instance Show Money where
    show (Amount x) = show x

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
    pv = fv / (1 + r) ^ n
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

