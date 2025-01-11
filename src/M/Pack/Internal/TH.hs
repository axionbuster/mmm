-- | Template Haskell code for shadowing 'Pack' and 'Unpack' instances
--
-- Used with "serde" quasiquoter
module M.Pack.Internal.TH
  ( derivegeneric,
    derivepack,
    deriveunpack,
    derivepackunpack,
    derivenothing,
  )
where

import Data.Functor
import Data.Serde.QQ
import GHC.Generics
import Language.Haskell.TH
import M.Pack.Internal.Types

-- | derive 'Generic' instance for a type
derivegeneric :: Q Type -> Q [Dec]
derivegeneric ty = [d|deriving instance Generic $(ty)|]

-- | shadow-derive a 'Pack' instance for a type
derivepack :: RunUserCoercion -> Q [Dec]
derivepack RunUserCoercion {..} = do
  [d|
    instance Pack $(datatyp) where
      pack $(patnormal) = pack $(appshadow)
      {-# INLINEABLE pack #-}
    |]

-- | shadow-derive an 'Unpack' instance for a type
deriveunpack :: RunUserCoercion -> Q [Dec]
deriveunpack RunUserCoercion {..} = do
  [d|
    instance Unpack $(datatyp) where
      unpack = unpack <&> \($(patshadow)) -> $(appnormal)
      {-# INLINEABLE unpack #-}
    |]

-- | shadow-derive 'Pack' and 'Unpack' instances for a type
derivepackunpack :: RunUserCoercion -> Q [Dec]
derivepackunpack RunUserCoercion {..} = do
  [d|
    instance Pack $(datatyp) where
      pack $(patnormal) = pack $(appshadow)
      {-# INLINEABLE pack #-}

    instance Unpack $(datatyp) where
      unpack = unpack <&> \($(patshadow)) -> $(appnormal)
      {-# INLINEABLE unpack #-}
    |]

-- | literally do nothing
derivenothing :: RunUserCoercion -> Q [Dec]
derivenothing _ = pure []
