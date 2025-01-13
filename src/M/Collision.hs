-- |
-- Module: M.Collision
-- Description: Unified collision detection system
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Re-exports both pure and effectful collision detection systems.
-- Provides a unified interface for the collision detection subsystem.
module M.Collision
  ( module M.Collision.Effectful,
    module M.Collision.Pure,
  )
where

import M.Collision.Effectful
import M.Collision.Pure
