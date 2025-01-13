-- |
-- Module: M.NBT
-- Description: Java NBT (Named Binary Tag) format implementation
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Provides types and functions for working with Minecraft's NBT format, which is
-- used for storing structured binary data.
module M.NBT (Ty (..), Tg (..), NamedPair (..)) where

import M.NBT.Internal.JS ()
import M.NBT.Internal.P
import M.NBT.Internal.Types
