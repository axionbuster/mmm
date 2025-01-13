{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module M.J.V769.S where

import Control.DeepSeq
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import GHC.Generics
import M.J.V769.I
import M.LEB
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
