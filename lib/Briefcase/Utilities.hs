{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.Utilities (
      percent
    , dollarAmount
    , multiplier
) where

import Formatting

import Briefcase.TimeValueOfMoney

import qualified Data.Text.Lazy.IO as L

--
-- | Given a number (ideally between 0.0 and 1.0) present it as a percentage.
--
percent :: Real a => Format r (a -> r)
percent =
    mapf (*100) (left 5 ' ' %. fixed 1) % "%"

--
-- | Take a Money value and, rounding it to a whole dollar amount, format it
-- as an amount of currency.
--
dollarAmount :: RealFrac a => Format r (a -> r)
dollarAmount =
    mapf dollars ("$" % commas)
  where
    -- annoyingly, had to specialize this; leaving it as
    -- RealFrac a -> Integral b wasn't good enough. Int32
    -- would represent over $2 billion, more than fine;
    -- and this is a 64 bit system.
    dollars :: RealFrac a => a -> Int
    dollars = round

dollarAmount0 :: Format r (Int -> Int -> r)
dollarAmount0 =
    "$" % commas % "." % left 2 ' '

--
-- | Given a number representing a ratio between two amounts,
-- present it as a ratio.
--
multiplier :: Real a => Format r (a -> r)
multiplier =
    left 5 ' ' %. fixed 1 % "x"

