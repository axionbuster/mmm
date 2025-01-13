{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: M.NBT.Internal.P
-- Description: NBT parsing and serialization
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements parsing and serialization for NBT data structures,
-- handling named pairs and tag types.
module M.NBT.Internal.P (NamedPair (..)) where

import Control.Applicative.Combinators
import Control.DeepSeq
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, byteString)
import Data.Data
import Data.Functor
import Data.HashMap.Strict qualified as M
import Data.Int
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import FlatParse.Stateful qualified as F
import GHC.Generics
import M.NBT.Internal.JS
import M.NBT.Internal.Types
import M.Pack

-- unpack orphan instance for 'Tg'

-- parser returns parser. use 'join' from Control.Monad to use it

-- | named pair of a 'Text' and a 'Tg'
data NamedPair = NamedPair !Text !Tg
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData)

-- NamedPair used to be called 'S', hence the 'sp' function name
sp :: NamedPair -> (Text, Tg)
sp (NamedPair t p) = (t, p)
{-# INLINE sp #-}

instance Unpack NamedPair where
  unpack = tag >>= liftA2 NamedPair string0
  {-# INLINE unpack #-}

instance Unpack Tg where
  unpack = join tag
  {-# INLINE unpack #-}

tag :: Parser st r (Parser st r Tg)
tag =
  unpack @Ty <&> \case
    TEnd -> pure End
    TByte -> Byte <$> unpack
    TShort -> Short <$> unpack
    TInt -> Int <$> unpack
    TLong -> Long <$> unpack
    TFloat -> Float <$> unpack
    TDouble -> Double <$> unpack
    TByteArray ->
      unpackfi @Int32
        >>= guardnat "Tg.ByteArray length"
        >>= (ByteArray <$>) . F.take
    TString -> String <$> string0
    TList -> do
      p <- F.lookahead tag
      ty <- unpack @Ty
      n <- unpackfi @Int32 >>= guardnat "Tg.List length"
      if ty == TEnd && n /= 0
        then F.err "only empty lists may have the end tag as element type"
        else List ty <$> V.replicateM n p
    TCompound -> Compound . M.fromList <$> manyTill (sp <$> unpack) end
      where
        end = unpack @Ty >>= guard . (== TEnd)
        {-# INLINE end #-}
    TIntArray -> IntArray <$> arr0 "Tg.IntArray length" (unpack @Int32)
    TLongArray -> LongArray <$> arr0 "Tg.LongArray length" (unpack @Int64)

arr0 :: (VU.Unbox a) => String -> Parser st r a -> Parser st r (VU.Vector a)
arr0 n p = unpackfi @Int32 >>= guardnat n >>= (`VU.replicateM` p)
{-# INLINE arr0 #-}

string0 :: Parser st r Text
string0 =
  unpackfi @Int16
    >>= guardnat "Tg.String length"
    >>= (getjs <$>) . (`F.isolate` fromcesu8p)
{-# INLINE string0 #-}

-- pack

instance Pack NamedPair where
  pack (NamedPair t p) = pack (getty p) <> spack t <> bodypack p
  {-# INLINE pack #-}

spack :: Text -> Builder
spack s =
  let c = tocesu8 (JS s)
   in packfi @Int16 (B.length c) <> byteString c
{-# INLINE spack #-}

instance Pack Tg where
  pack t = pack (getty t) <> bodypack t
  {-# INLINE pack #-}

bodypack :: Tg -> Builder
bodypack End = mempty
bodypack (Byte p) = pack p
bodypack (Short p) = pack p
bodypack (Int p) = pack p
bodypack (Long p) = pack p
bodypack (Float p) = pack p
bodypack (Double p) = pack p
bodypack (ByteArray p) = packfi @Int32 (B.length p) <> byteString p
bodypack (String p) = spack p
bodypack (List t p) =
  pack t
    <> packfi @Int32 (V.length p)
    <> V.foldMap bodypack p
bodypack (Compound p) =
  foldMap
    (pack . uncurry NamedPair)
    (M.toList p)
    <> pack TEnd
bodypack (IntArray p) = packfi @Int32 (VU.length p) <> VU.foldMap pack p
bodypack (LongArray p) = packfi @Int32 (VU.length p) <> VU.foldMap pack p
