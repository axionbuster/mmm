module M.V769.Reg (handshake) where

import M.IO.TH
import M.V769.H qualified as H

[states|
  -- name
  handshake
  -- name:hex recv:hex send
  H.HandshakePacket::0
  |]
