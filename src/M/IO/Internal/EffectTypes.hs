-- |
-- Module: M.IO.Internal.EffectTypes
-- Description: Core networking effects for Minecraft protocol
-- License: BSD-3-Clause
--
-- This module defines the core effects used for networking in the Minecraft protocol
-- implementation. It provides bidirectional packet communication with compression and
-- encryption support.
module M.IO.Internal.EffectTypes
  ( -- * Core effect
    Talking (..),

    -- * Types
    Direction (..),
    Immediately (..),
    Op (..),
    ParserState (..),

    -- * Effect operations
    Talking',
    hear,
    hearU,
    hearA,
    say,

    -- * Configuration
    setcompression,
    setencryption,
    enter,
  )
where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Hashable
import Data.Word
import Effectful
import Effectful.NonDet
import Effectful.State.Dynamic
import Effectful.TH
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.IO.Internal.Datagram
import M.Pack

-- | relative packet direction. Used to identify packet flow without
-- hardcoding client/server roles
data Direction = Inbound | Outbound
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable, Generic, Lift)
  deriving (Hashable, NFData)

-- | urgency level for receiving packets
data Immediately = Immediately | Eventually
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable, Generic, Lift)
  deriving (Hashable, NFData)

-- | operations on a packet
data Op r where
  -- | parse any packet
  Parse :: Uninterpreted %1 -> Op SomeUnpack
  -- | find code of a packet based on its 'TypeRep'
  Code :: Direction %1 -> TypeRep %1 -> Op (Maybe Word8)
  deriving (Typeable)

instance Show (Op r) where
  show = \case
    Parse u -> "Parse " ++ show u
    Code d t -> "Code " ++ show d ++ " " ++ show t

instance Eq (Op r) where
  Parse u == Parse u' = u == u'
  Code d t == Code d' t' = d == d' && t == t'

instance Ord (Op r) where
  compare (Parse u) (Parse u') = compare u u'
  compare (Code d t) (Code d' t') = compare (d, t) (d', t')

instance Hashable (Op r) where
  hashWithSalt s (Parse u) = s `hashWithSalt` (0 :: Int) `hashWithSalt` u
  hashWithSalt s (Code d t) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` d
      `hashWithSalt` t

instance NFData (Op r) where
  rnf = \case
    Parse u -> rnf u
    Code d t -> rnf d `seq` rnf t

-- | parser state object (as in object-oriented programming)
newtype ParserState = ParserState
  { -- | send a \"message\" to the parser state and get a response
    send2parserstate :: forall r. Op r -> r
  }

-- | the communication effect
data Talking :: Effect where
  -- | listen for a message and assert its type
  --
  -- when immediately is set and message missing, invoke 'Empty'
  Hear :: (Unpack a, Typeable a) => Immediately -> Talking m a
  -- | listen for a raw uninterpreted message
  --
  -- when immediately is set and message missing, invoke 'Empty'
  HearU :: Immediately -> Talking m Uninterpreted
  -- | listen for a message with dynamic unpacking
  --
  -- when immediately is set and message missing, invoke 'Empty'
  HearA :: Immediately -> Talking m SomeUnpack
  -- | send a message
  Say :: (Pack a, Typeable a) => a -> Talking m ()
  -- | set the compression threshold
  --
  -- - non-negative: compress messages larger than this size
  -- - negative: disable compression
  Setcompression :: Int -> Talking m ()
  -- | set the encryption key
  Setencryption :: ByteString -> Talking m ()

makeEffect ''Talking

-- | the communication effect with parser state and non-determinism
type Talking' es = (Talking :> es, State ParserState :> es, NonDet :> es)

-- | enter the parser state
enter :: (State ParserState :> es) => ParserState -> Eff es ()
enter = put
{-# INLINE enter #-}
