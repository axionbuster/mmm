{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: M.V769.S
-- Description: Protocol status state packets (AI-generated)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines packet structures for the status state of protocol version 769,
-- handling server list ping functionality including server status queries
-- and latency checks.
module M.V769.S where

import Control.DeepSeq
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import GHC.Generics
import M.Pack

[serde|
.derive
  Show Read Data Typeable

data StatusResponse
  jsonresponse :: Text

data StatusRequest

data PongResponse
  payload :: Int64
  |]

-- provided by "th-serde": Data.Serde.QQ
runusercoercion
  -- provided by M.Pack
  borrowderivepackunpack
  properderivepackunpack
  -- preparations for shadow types
  [ ''Generic,
    ''NFData,
    ''Eq,
    ''Ord
  ]
