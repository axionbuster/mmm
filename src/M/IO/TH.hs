module M.IO.TH (ParserStates (..), thparserstates) where

import Data.HashMap.Strict qualified as H
import Data.IntMap.Strict qualified as I
import Language.Haskell.TH hiding (Code)
import Language.Haskell.TH.Lib
import M.IO.Internal.Datagram
import M.IO.Internal.EffectTypes
import M.Pack
import Type.Reflection (SomeTypeRep (..))

data ParserStates = ParserStates
  { forserver :: ParserState,
    forclient :: ParserState
  }

type M = Maybe

data S = S
  { sna :: String,
    sic :: M Int,
    soc :: M Int,
    sis :: M Int,
    sos :: M Int
  }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

forceindex :: I.IntMap a -> Int -> a
forceindex = (I.!)

concat2 :: [a] -> [a] -> [a]
concat2 = (++)

thparserstates :: [S] -> Q Exp
thparserstates rows = do
  let genparse l =
        let f s a
              | Just i <- l s =
                  (conT $ mkName $ sna s, litE . IntegerL . fi $ i) : a
              | otherwise = a
            pairs = foldr f [] rows
         in appE (varE 'I.fromList) do
              listE $ flip map pairs \(ty, co) ->
                tupE [co, appTypeE (varE 'unpacksome) ty]
      gencode li lo =
        let f l s a
              | Just i <- l s =
                  let t = mkName $ sna s
                   in ( appTypeE (varE 'SomeTypeRep) (varT t),
                        litE . IntegerL . fi $ i
                      )
                        : a
              | otherwise = a
            inbound = foldr (f li) [] rows
            outbound = foldr (f lo) [] rows
            u l i = listE $ flip map l \(tr, co) -> tupE [tupE [varE i, tr], co]
         in appE (varE 'H.fromList) do
              appE
                ( appE
                    (varE 'concat2)
                    (u inbound 'Inbound)
                )
                (u outbound 'Outbound)
      gensparse = genparse sis
      gencparse = genparse sic
      genscode = gencode sis sos
      genccode = gencode sis soc
  argname <- newName "argname" -- some temporary binder
  u <- newName "u" -- uninterpreted (essentially code * rest of packet)
  a <- newName "a" -- the parsing list
  b <- newName "b" -- the direction * type -> code list
  d <- newName "d" -- direction
  y <- newName "y" -- type
  let half g0 g1 =
        letE [valD (varP a) (normalB g0) [], valD (varP b) (normalB g1) []] do
          lam1E (varP argname) do
            caseE
              (varE argname)
              [ match
                  (conP 'Parse [varP u])
                  ( normalB do
                      appE
                        ( appE
                            (varE 'parsepure0)
                            ( appE
                                (appE (varE 'forceindex) (varE a))
                                (appE (varE 'pkcode) (varE u))
                            )
                        )
                        (appE (varE 'pkdata) (varE u))
                  )
                  [],
                match
                  (conP 'Code [varP d, varP y])
                  ( normalB do
                      appE
                        ( appE
                            (varE 'H.lookup)
                            (varE b)
                        )
                        (tupE [varE d, varE y])
                  )
                  []
              ]
  tupE [half gensparse genscode, half gencparse genccode]
