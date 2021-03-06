{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune #-}

module Briefcase.CashFlow
    ( Grouping(..)
    , grouping
    , rangeOverGrouping
    , totalOverGrouping
    , CashFlow(..)
    , total
    , range
    , annually
    , quarterly
    , monthly
    , fortnightly
    , weekly
    , series

    -- for testing only
    , rangeDates
    , quarterlyDates
    , monthlyDates
    , fortnightlyDates
    )
where

import Data.Foldable (foldl')
import Data.Hourglass (Date(..), Period(..), dateAddPeriod)

import Core.Text
import Briefcase.Money

data Grouping = Grouping
    { labelOf :: Rope
    , flowsIn :: [[CashFlow]]
    }
    deriving (Show)

grouping :: Rope -> [[CashFlow]] -> Grouping
grouping = Grouping

totalOverGrouping :: Grouping -> Money
totalOverGrouping (Grouping _ flows) = foldl' (+) 0.00 . fmap total $ flows

rangeOverGrouping :: Date -> Date -> Grouping -> Grouping
rangeOverGrouping from unto (Grouping label flows) =
  let
    flows' = fmap (range from unto) flows
  in
    Grouping label flows'


data CashFlow = CashFlow
    { nameOf :: Rope
    , amountOf :: Money
    , dateOf :: Date
    }
    deriving (Show, Eq)

{-|
Given a /finite/ list of CashFlows, sum the amounts
-}
total :: [CashFlow] -> Money
total flows = foldl' f 0.00 flows
  where
    f balance flow = balance + amountOf flow

{-|
Given an (infinite) list of CashFlows, extract the flows that are within
the specified date range. Relies on the assumption that the list of flows
is in ascending order.
-}
-- duplicates code originally developed in rangeDates. Be nicer if we could
-- use that instead.
range :: Date -> Date -> [CashFlow] -> [CashFlow]
range from unto list = start list
  where
    start [] = []
    start (flow:flows) = if (from > dateOf flow)
        then start flows
        else finish (flow:flows)

    finish [] = []
    finish (flow:flows) = if dateOf flow > unto
        then []
        else flow : finish flows


{-|
A cash flow that repeats every year.
-}
annually :: Rope -> Money -> Date -> [CashFlow]
annually = stream annuallyDates

{-|
A cash flow that repeats quarterly.
-}
quarterly :: Rope -> Money -> Date -> [CashFlow]
quarterly = stream quarterlyDates

{-|
A cash flow that repeats monthly.
-}
monthly :: Rope -> Money -> Date -> [CashFlow]
monthly = stream monthlyDates

{-|
A cash flow that repeats fortnightly.
-}
fortnightly :: Rope -> Money -> Date -> [CashFlow]
fortnightly = stream fortnightlyDates

{-|
A cash flow that repeats every week.
-}
weekly :: Rope -> Money -> Date -> [CashFlow]
weekly = stream weeklyDates


stream :: (Date -> [Date]) -> Rope -> Money -> Date -> [CashFlow]
stream f name amount seed =
  let
    dates = f seed
  in
    fmap (\date -> CashFlow { dateOf = date, amountOf = amount, nameOf = name }) dates

{-|
A repeating list of cashflows with specific dates across a year. The flows will be
replicated with the same dates in subsequent years.
-}
series :: Rope -> [(Money,Date)] -> [CashFlow]
series name pairs =
  let
    flows = fmap (\(amount,date) -> CashFlow name amount date) pairs
    delta = Period 1 0 0 -- 1 year
    pairs' = fmap (\(amount,date) -> (amount,dateAddPeriod date delta)) pairs
  in
    flows ++ series name pairs'



-- Extract a range of dates from a list. This would be useful to say "the
-- cash flows for next year", thus bridging between a known time range and
-- infinite lists generated by the repeating functions below
rangeDates :: Date -> Date -> [Date] -> [Date]
rangeDates from unto list = start list
  where
    start [] = []
    start (date:dates) = if (from > date)
        then start dates
        else finish (date:dates)

    finish [] = []
    finish (x:xs) = if x > unto
        then []
        else x : finish xs


annuallyDates :: Date -> [Date]
annuallyDates = increment (Period 1 0 0)

quarterlyDates :: Date -> [Date]
quarterlyDates = increment (Period 0 3 0)

monthlyDates :: Date -> [Date]
monthlyDates = increment (Period 0 1 0)

fortnightlyDates :: Date -> [Date]
fortnightlyDates = increment (Period 0 0 14)

weeklyDates :: Date -> [Date]
weeklyDates = increment (Period 0 0 7)

{-|
Return an infinite lists of dates incrementing by the period specified,
starting from (and including) the given seed date.
-}
increment :: Period -> Date -> [Date]
increment delta from =
  let
    next = dateAddPeriod from delta
  in
    from : increment delta next
