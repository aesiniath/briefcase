{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.TimeValue
  ( FutureValue
  , PresentValue
  , Rate
  , Periods
  , discount
  , futureValue
  , presentValue
  , netPresentValue
  )
where

import Data.Fixed
import Data.Foldable (foldl')

import Briefcase.Money

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

