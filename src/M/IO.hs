-- |
-- Module: M.IO
-- Description: Core input/output operations for Minecraft protocol
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Provides high-level IO operations and types for handling Minecraft protocol
-- connections, including socket management and data streaming.
module M.IO
  ( Connection (..),
    Uninterpreted (..),
    -- | define communication effects
    module M.IO.Internal.EffectTypes,
    withtalkingserver,
    withcxfromsocket,
  )
where

import M.IO.Internal.Datagram
import M.IO.Internal.EffectSocket
import M.IO.Internal.EffectTypes
import M.IO.Internal.Socket
