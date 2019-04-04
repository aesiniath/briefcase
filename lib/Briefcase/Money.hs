{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.Money
  ( Money( )
  , money
  )
where

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
