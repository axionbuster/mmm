-- |
-- Module: M.IO.Obs
-- Description: General observer pattern implementation
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module provides a general-purpose observer pattern implementation
-- for monitoring and reacting to state changes in STM transactions.
module M.IO.Obs
  ( -- * Observer
    obs,
  )
where

import Control.Monad
import UnliftIO

-- | A general observer that monitors changes in a shared variable and reacts to them.
--
-- The observer watches a target variable for changes, compares values using a custom
-- comparison function, and executes an action when changes are detected.
--
-- @
-- obs targetvar              -- Variable to observe
--     compareandtransform    -- STM function to compare and transform values
--     reacttochange          -- Action to execute when changes occur
-- @
obs ::
  (MonadIO m, Eq a) =>
  -- | Target variable to observe for changes
  TVar a ->
  -- | Function to compare and transform old and new values in STM
  --
  -- Old value is passed first; returned value gets committed and remembered
  (a -> a -> STM a) ->
  -- | Action to execute when changes are detected, with old and new values
  --
  -- Old value is passed first
  (a -> a -> m ()) ->
  m b
obs a b c = do
  old <- readTVarIO a >>= newTVarIO
  forever do
    (ex, ez) <- atomically do
      x <- readTVar old
      y <- readTVar a
      guard (x /= y)
      z <- b x y
      writeTVar old z
      pure (x, z)
    c ex ez
