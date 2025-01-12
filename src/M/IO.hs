-- | Input and output operations.
module M.IO
  ( Connection (..),
    Uninterpreted (..),
    -- | define communication effects
    module M.IO.Internal.EffectTypes,
    withtalkingserver,
    withtalkingclient,
    withcxfromsocket,
  )
where

import M.IO.Internal.Datagram
import M.IO.Internal.EffectSocket
import M.IO.Internal.EffectTypes
import M.IO.Internal.Socket
