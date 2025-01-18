module M.V769.Reg (handshake) where

import M.IO.TH
import M.V769.H qualified as H

[states|
  handshake
  H.HandshakePacket::0
  |]
