-- |
-- Module: M.Pack.Internal.Types
-- Description: Core serialization type system
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines the core type system for binary serialization and deserialization,
-- including Pack and Unpack typeclasses and related utilities.
module M.Pack.Internal.Types
  ( Pack (..),
    SomePack (..),
    Unpack (..),
    SomeUnpack (..),
    Parser,
    Result,
    ParseError (..),
    parsepure,
    parsepure0,
    castsomepack,
    castsomeunpack,
    unpacksome,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.ST.Strict
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Data
import Data.String
import Data.Void
import FlatParse.Stateful qualified as F
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)

-- | our parser type
type Parser st r = F.ParserT st r ParseError

-- | our parse result
type Result = F.Result ParseError

-- | our parser error type
newtype ParseError = ParseError {showparseerror :: String} -- generic message
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid)
  deriving stock (Typeable, Data, Generic, Lift)
  deriving anyclass (Exception)

-- | run a pure parser efficiently (by inlining)
parsepure :: (forall st. Parser st r a) -> r -> Int -> ByteString -> Result a
parsepure p r i b = runST (F.runParserST p r i b)
{-# INLINE parsepure #-}

-- | run a pure parser with no state efficiently (by inlining)
parsepure0 :: (forall st. Parser st () a) -> ByteString -> Result a
parsepure0 p = parsepure p () 0
{-# INLINE parsepure0 #-}

-- | produce a 'Builder' from a value
class Pack a where
  -- | produce a 'Builder' from a value
  pack :: a -> Builder
  default pack :: (Generic a, GPack (Rep a)) => a -> Builder
  pack = gpack . from
  {-# INLINE pack #-}

-- | existential 'Pack' container
data SomePack = forall a. (Typeable a, Pack a, Show a) => SomePack a
  deriving stock (Typeable)

instance Pack SomePack where
  pack (SomePack x) = pack x
  {-# INLINE pack #-}

instance Show SomePack where
  show (SomePack x) = show x
  {-# INLINE show #-}

-- | cast a 'SomePack' to a type
castsomepack :: (Typeable a) => SomePack -> Maybe a
castsomepack (SomePack a) = cast a
{-# INLINE castsomepack #-}

-- | retrieve a 'Parser' for a type
class Unpack a where
  -- | retrieve a 'Parser' for a type
  unpack :: Parser st r a
  default unpack :: (Generic a, GUnpack (Rep a)) => Parser st r a
  unpack = to <$> gunpack
  {-# INLINE unpack #-}

-- | existential 'Unpack' container
--
-- to 'unpack' this, try using 'unpacksome' with a type application
data SomeUnpack = forall a. (Typeable a, Unpack a, Show a) => SomeUnpack a
  deriving stock (Typeable)

instance Show SomeUnpack where
  show (SomeUnpack x) = show x
  {-# INLINE show #-}

-- | use a type application to retrieve a 'SomeUnpack'
unpacksome ::
  forall a st r.
  (Typeable a, Unpack a, Show a) =>
  Parser st r SomeUnpack
unpacksome = SomeUnpack <$> unpack @a
{-# INLINE unpacksome #-}

-- | cast a 'SomeUnpack' to a type
castsomeunpack :: (Typeable a) => SomeUnpack -> Maybe a
castsomeunpack (SomeUnpack a) = cast a
{-# INLINE castsomeunpack #-}

-- | generic implementor of 'Pack'
class GPack f where
  -- | implement 'pack'
  gpack :: f a -> Builder

-- GPack is implemented for most type combos, except sum types
-- where programmer's attention is required

instance GPack V1 where
  gpack v = case v of {}
  {-# INLINE gpack #-}

instance GPack U1 where
  gpack _ = mempty
  {-# INLINE gpack #-}

instance (Pack a) => GPack (K1 i a) where
  gpack (K1 x) = pack x
  {-# INLINE gpack #-}

instance (GPack f) => GPack (M1 i c f) where
  gpack (M1 x) = gpack x
  {-# INLINE gpack #-}

instance (GPack f, GPack g) => GPack (f :*: g) where
  gpack (x :*: y) = gpack x <> gpack y
  {-# INLINE gpack #-}

-- | generic implementor of 'Unpack'
class GUnpack f where
  -- | implement 'unpack'
  gunpack :: Parser st r (f a)

instance GUnpack V1 where
  gunpack = F.err "impossible (GUnpack V1)"
  {-# INLINE gunpack #-}

instance GUnpack U1 where
  gunpack = pure U1
  {-# INLINE gunpack #-}

instance (Unpack a) => GUnpack (K1 i a) where
  gunpack = K1 <$> unpack
  {-# INLINE gunpack #-}

instance (GUnpack f) => GUnpack (M1 i c f) where
  gunpack = M1 <$> gunpack
  {-# INLINE gunpack #-}

instance (GUnpack f, GUnpack g) => GUnpack (f :*: g) where
  gunpack = liftA2 (:*:) gunpack gunpack
  {-# INLINE gunpack #-}

-- some very basic instances

instance Pack Void where
  pack = absurd
  {-# INLINE pack #-}

instance Unpack Void where
  unpack = F.err "impossible (Unpack Void)"
  {-# INLINE unpack #-}

instance Pack () where
  pack _ = mempty
  {-# INLINE pack #-}

instance Unpack () where
  unpack = pure ()
  {-# INLINE unpack #-}

-- tuples up to five

instance (Pack a, Pack b) => Pack (a, b) where
  pack (a, b) = pack a <> pack b
  {-# INLINE pack #-}

instance (Unpack a, Unpack b) => Unpack (a, b) where
  unpack = liftA2 (,) unpack unpack
  {-# INLINE unpack #-}

instance (Pack a, Pack b, Pack c) => Pack (a, b, c) where
  pack (a, b, c) = pack a <> pack b <> pack c
  {-# INLINE pack #-}

instance (Unpack a, Unpack b, Unpack c) => Unpack (a, b, c) where
  unpack = liftA3 (,,) unpack unpack unpack
  {-# INLINE unpack #-}

instance (Pack a, Pack b, Pack c, Pack d) => Pack (a, b, c, d) where
  pack (a, b, c, d) = pack a <> pack b <> pack c <> pack d
  {-# INLINE pack #-}

instance (Unpack a, Unpack b, Unpack c, Unpack d) => Unpack (a, b, c, d) where
  unpack = (,,,) <$> unpack <*> unpack <*> unpack <*> unpack
  {-# INLINE unpack #-}

instance (Pack a, Pack b, Pack c, Pack d, Pack e) => Pack (a, b, c, d, e) where
  pack (a, b, c, d, e) = pack a <> pack b <> pack c <> pack d <> pack e
  {-# INLINE pack #-}

instance
  (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e) =>
  Unpack (a, b, c, d, e)
  where
  unpack = (,,,,) <$> unpack <*> unpack <*> unpack <*> unpack <*> unpack
  {-# INLINE unpack #-}
