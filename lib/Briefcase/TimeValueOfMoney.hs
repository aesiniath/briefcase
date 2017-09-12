{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.TimeValueOfMoney (
    futureValue,
    presentValue
) where

import Data.Fixed

type Money = Centi

type PresentValue = Money
type FutureValue = Money

type Rate = Double

type Periods = Int

--
-- | Given a stream of cash flows (assumed for now to be annual), calculate the
-- present value of stream
--
presentValue :: Rate -> Periods -> PresentValue -> FutureValue
presentValue rate periods fv =
  let
    r = realToFrac rate
    n = periods
  in
    fv / ((1 + r) ^ n)

futureValue :: Rate -> Periods -> PresentValue -> FutureValue
futureValue rate periods pv =
  let
    r = realToFrac rate
    n = periods
  in
    pv * ((1 + r) ^ n)

