{-# OPTIONS_GHC -Wno-orphans #-}

-- | Other implementations of 'Pack' and 'Unpack'
module M.Pack.Internal.Etc () where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.UUID.Types
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word
import FlatParse.Stateful qualified as F
import M.Pack.Internal.Num
import M.Pack.Internal.Types

instance Pack Text where
  pack = pack . TE.encodeUtf8
  {-# INLINEABLE pack #-}

instance Unpack Text where
  unpack =
    TE.decodeUtf8' <$> unpack @ByteString >>= \case
      Left err -> F.err (ParseError (show err))
      Right t -> pure t
  {-# INLINEABLE unpack #-}

instance Pack ByteString where
  pack b = packleb32 (B.length b) <> byteString b
  {-# INLINEABLE pack #-}

instance Unpack ByteString where
  unpack = unpackleb32 >>= F.take
  {-# INLINE unpack #-}

instance (Pack a) => Pack (Maybe a) where
  pack = \case
    Nothing -> pack @Word8 0
    Just x -> pack @Word8 1 <> pack x
  {-# INLINEABLE pack #-}

instance (Unpack a) => Unpack (Maybe a) where
  unpack =
    F.anyWord8 >>= \case
      0 -> pure Nothing
      1 -> Just <$> unpack
      n -> F.err $ ParseError $ "Maybe: invalid tag: " <> show n
  {-# INLINEABLE unpack #-}

-- vector

instance (Pack a) => Pack (V.Vector a) where
  pack = packleb32 . V.length <> V.foldMap' pack
  {-# INLINEABLE pack #-}

instance (Unpack a) => Unpack (V.Vector a) where
  unpack = unpackleb32 >>= flip V.replicateM unpack
  {-# INLINEABLE unpack #-}

instance (VU.Unbox a, Pack a) => Pack (VU.Vector a) where
  pack = packleb32 . VU.length <> VU.foldMap' pack
  {-# INLINEABLE pack #-}

instance (VU.Unbox a, Unpack a) => Unpack (VU.Vector a) where
  unpack = unpackleb32 >>= flip VU.replicateM unpack
  {-# INLINEABLE unpack #-}

-- UUID

instance Pack UUID where
  pack = pack . toWords64
  {-# INLINE pack #-}

instance Unpack UUID where
  unpack = fromWords64 <$> unpack <*> unpack
  {-# INLINE unpack #-}
