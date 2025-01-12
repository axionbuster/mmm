-- | Networking effects for Minecraft clients and servers.
module M.IO.Effect
  ( Talking (..),
    Direction (..),
    Op (..),
    ParserState (..),
    hear,
    hearU,
    hearA,
    say,
    setcompression,
    setencryption,
    enter,
  )
where

import Data.ByteString (ByteString)
import Data.Data
import Data.Word
import Effectful
import Effectful.TH
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.IO.Internal.Datagram
import M.Pack

-- | relative direction of a packet
--
-- why relative? because the same mechanism is shared by
-- both the client and the server, so we don't want to
-- hardcode the direction of a packet
data Direction = Inbound | Outbound
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable, Generic, Lift)

-- | operations on a packet
data Op r where
  -- | parse any packet
  Parse :: Uninterpreted %1 -> Op (IO SomeUnpack)
  -- | find code of a packet based on its 'TypeRep'
  Code :: Direction %1 -> TypeRep %1 -> Op (Maybe Word8)
  deriving (Typeable)

-- | parser state object (as in object-oriented programming)
newtype ParserState = ParserState
  { -- | send a \"message\" to the parser state and get a response
    send2parserstate :: forall r. Op r -> IO r
  }

-- | the communication effect
data Talking :: Effect where
  -- | listen for a message that can be unpacked into type @a@
  Hear :: (Unpack a) => Talking m a
  -- | listen for a raw uninterpreted message
  HearU :: Talking m Uninterpreted
  -- | listen for a message with dynamic unpacking
  HearA :: Talking m SomeUnpack
  -- | send a packable message of type @a@
  Say :: (Pack a) => a -> Talking m ()
  -- | set the compression threshold
  --
  -- - positive: compress messages larger than this size
  -- - non-positive: disable compression
  Setcompression :: Int -> Talking m ()
  -- | set the encryption key
  Setencryption :: ByteString -> Talking m ()
  -- | change the parser state; enter a new state
  Enter :: ParserState -> Talking m ()

makeEffect ''Talking
