-- | Input and output operations.
module M.IO
  ( Connection (..),
    Uninterpreted (..),
    module M.IO.Effect,
    withcxfromsocket,
  )
where

import M.IO.Effect
import M.IO.Internal.Datagram
import M.IO.Internal.Socket
