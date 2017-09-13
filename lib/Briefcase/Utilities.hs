{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Briefcase.Utilities (
      percent
) where

import Formatting

import qualified Data.Text.Lazy.IO as L

--
-- | Given a number (ideally between 0.0 and 1.0) present it as a percentage.
--
percent :: Real a => Format r (a -> r)
percent =
    mapf (*100) (left 5 ' ' %. fixed 1) % "%"
