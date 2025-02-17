{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: M.V769.H
-- Description: Protocol handshake packets (AI-generated)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements packet structures for the initial handshake phase of protocol
-- version 769, where clients establish basic connection parameters including
-- protocol version and intended next state.
module M.V769.H where

import Control.DeepSeq
import Data.Data
import Data.Int
import Data.Text (Text)
import Data.Word
import M.LEB
import M.PkMacro

setdefaultderives
addproperderives [''NFData, ''Typeable, ''Show, ''Eq]

[pkmacro|
data HandshakePacket {
  protocolversion :: Int32 via VarInt,
  serveraddress :: Text,
  serverport :: Word16,
  nextstate :: Int32 via VarInt,
}
  |]
