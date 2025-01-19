-- |
-- Module: M.IO.Tick
-- Description: Timing control for periodic operations
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module provides functionality for running actions at controlled intervals
-- with adaptive timing adjustments.
module M.IO.Tick
  ( -- * Timing Control
    tick,
  )
where

import Control.Monad
import Data.Functor
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.State.Static.Local
import GHC.Clock

-- | Executes an action periodically with adaptive timing control.
--
-- The function ensures the action runs at a target frequency by adjusting
-- delays between executions based on execution time.
--
-- @
-- tick ratevar action    -- Runs \'action\' at frequency specified by rateVar (in Hz)
-- @
tick ::
  (IOE :> es, Concurrent :> es) =>
  -- | Target frequency in Hz (stored in 'TVar')
  TVar Double ->
  -- | Action to execute periodically
  Eff es () ->
  Eff es b
tick tr f = do
  a <- liftIO getMonotonicTime -- s
  evalState a do
    forever do
      raise f
      t0 <- get
      t1 <- liftIO getMonotonicTime
      wa <- readTVarIO tr <&> \t -> max 0 (recip t - (t1 - t0))
      threadDelay (ceiling (1e6 * wa)) -- 1e6 us = (1e6 us/s) * s = s
