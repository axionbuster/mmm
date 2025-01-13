-- |
-- Module: M.Pack
-- Description: Core serialization framework
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Provides the core serialization framework including Pack and Unpack typeclasses,
-- along with utilities for numeric types, newtypes, bit operations, and Template
-- Haskell support for automated instance derivation.
module M.Pack
  ( -- | 'Pack' and 'Unpack' classes for serializing/deserializing data
    module M.Pack.Internal.Types,
    -- | instances for numbers; utilities for packing and unpacking numbers
    -- in specific formats; instances for 'Bool'
    module M.Pack.Internal.Num,
    -- | newtypes for modulating serialization behavior
    module M.Pack.Internal.Newtypes,
    -- | bit-fiddling: bitsets, bitflags, etc.
    module M.Pack.Internal.Bit,
    -- | template Haskell utilities for use with "th-serde"
    -- to help derive 'Pack' and 'Unpack' instances
    module M.Pack.Internal.TH,
  )
where

import M.Pack.Internal.Bit
import M.Pack.Internal.Etc ()
import M.Pack.Internal.Linear ()
import M.Pack.Internal.Newtypes
import M.Pack.Internal.Num
import M.Pack.Internal.TH
import M.Pack.Internal.Types
