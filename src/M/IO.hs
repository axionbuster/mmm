-- | Input and output operations.
module M.IO
  ( Connection (..),
    Uninterpreted (..),
    -- | define communication effects
    module M.IO.Internal.EffectTypes,
    withcxfromsocket,
  )
where

import M.IO.Internal.Datagram
import M.IO.Internal.EffectTypes
import M.IO.Internal.Socket
