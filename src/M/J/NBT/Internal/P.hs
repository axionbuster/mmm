{-# OPTIONS_GHC -Wno-orphans #-}

-- | parse / serialize NBT data
module M.J.NBT.Internal.P () where

import Control.Applicative.Combinators
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.Functor
import Data.HashMap.Strict qualified as M
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import FlatParse.Stateful qualified as F
import M.J.NBT.Internal.JS
import M.J.NBT.Internal.Types
import M.Pack

-- unpack orphan instance for 'Tg'

-- parser returns parser. use 'join' from Control.Monad to use it

-- | named pair of a 'Text' and a 'Tg'
data S = S !Text !Tg

sp :: S -> (Text, Tg)
sp (S t p) = (t, p)
{-# INLINE sp #-}

instance Unpack S where
  unpack = tag >>= \p -> S <$> string0 <*> p
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
    TByteArray -> unpackfi @Int32 >>= (ByteArray <$>) . F.take
    TString -> String <$> string0
    TList -> do
      p <- tag
      n <- unpackfi @Int32
      V.replicateM n p <&> mklist1 >>= \case
        Just l -> pure l
        Nothing -> F.err "tag: empty or End tag-containing list"
    TCompound -> Compound . M.fromList <$> manyTill (sp <$> unpack) end
      where
        end = unpack @Ty >>= guard . (== TEnd)
        {-# INLINE end #-}
    TIntArray -> IntArray <$> arr0 (unpack @Int32)
    TLongArray -> LongArray <$> arr0 (unpack @Int64)

arr0 :: (VU.Unbox a) => Parser st r a -> Parser st r (VU.Vector a)
arr0 p = unpackfi @Int32 >>= (`VU.replicateM` p)
{-# INLINE arr0 #-}

string0 :: Parser st r Text
string0 =
  unpackfi @Int16 >>= (cesu8astext <$>) . F.take >>= \case
    Just t -> pure t
    Nothing -> F.err "string0: invalid CESU-8"

-- pack

instance Pack S where
  pack (S t p) = pack (getty p) <> spack t <> pack p
  {-# INLINE pack #-}

spack :: Text -> Builder
spack s = packfi @Int16 (T.length s) <> pack (JS s)
{-# INLINE spack #-}

instance Pack Tg where
  pack End = mempty
  pack (Byte p) = pack p
  pack (Short p) = pack p
  pack (Int p) = pack p
  pack (Long p) = pack p
  pack (Float p) = pack p
  pack (Double p) = pack p
  pack (ByteArray p) = packfi @Int32 (B.length p) <> pack p
  pack (String p) = spack p
  pack (List _ p) | V.null p = pack TEnd
  pack (List t p) =
    pack t
      <> packfi @Int32 (V.length p)
      <> V.foldMap pack p
  pack (Compound p) = foldMap (pack . uncurry S) (M.toList p) <> pack TEnd
  pack (IntArray p) = packfi @Int32 (VU.length p) <> VU.foldMap pack p
  pack (LongArray p) = packfi @Int32 (VU.length p) <> VU.foldMap pack p
