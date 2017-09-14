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
  , discount
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
-- Went through several evolutions of picking a type for this. It's partly
-- meant to enforce correct rounding behaviour of tenths of a cent, but also to
-- enable testing and direct use cases with hard coded values in code. Haskell
-- puts decimal literals as :: Fractional a => a. After a long spell with
-- Double, just started using Haskell's ratio machinery.
--
money :: Rational -> Money
money x =
  let
    cents  = x * 100
    cents' = round cents :: Integer
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

type Rate = Rational

type Periods = Int
type Year = Int


--
-- | Reduce a value by a given percentage.
--
discount :: Money -> Rational -> Money
discount value rate =
  let
    v = toRational value
    r = toRational rate

    v' = v * (1 - r)
  in
    money v'

--
-- | Future value of an amount, discounted over time. You rarely ever need
-- this, but conceptually it is the foundation of the rest of time value of
-- money.
--
futureValue :: Rate -> Periods -> PresentValue -> FutureValue
futureValue rate periods present =
  let
    r = toRational rate
    n = periods
    pv = toRational present

    fv = pv * (1 + r) ^ n
  in
    money fv

presentValue :: Rate -> Periods -> FutureValue -> PresentValue
presentValue rate periods future =
  let
    r = toRational rate
    n = periods
    fv = toRational future

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

