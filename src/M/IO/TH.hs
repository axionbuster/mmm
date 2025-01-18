-- |
-- Module: M.IO.TH
-- Description: Template Haskell generators for packet parsing states
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module provides Template Haskell functionality to generate parser states
-- for client-server packet handling. It uses a simple grammar to define packet
-- mappings and their associated codes.
--
-- == Usage
--
-- Define parser states using the quasi-quoter:
--
-- @
-- -- creates mystatepair :: ('ParserState', 'ParserState')
-- [states|
--   mystatepair
--   Login:1f:2f     -- Login packet: recv=0x1f, send=0x2f
--   Handshake::3f   -- Handshake packet: send=0x3f only
--   |]
-- @
--
-- The first state is for servers, the second for clients.
--
-- == Note
--
-- All numerals are hexadecimal.
module M.IO.TH (ParserStates (..), states) where

import Control.Applicative.Combinators (skipManyTill)
import Control.Monad
import Control.Monad.Fix
import Data.Char (isLetter, ord)
import Data.Function
import Data.Functor
import Data.HashMap.Strict qualified as H
import Data.IntMap.Strict qualified as I
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import FlatParse.Stateful hiding (Parser)
import Language.Haskell.TH hiding (Code)
import Language.Haskell.TH.Quote
import M.IO.Internal.Datagram
import M.IO.Internal.EffectTypes
import M.Pack
import Type.Reflection (SomeTypeRep (..))

-- | A quasi-quoter for generating parser states.
-- Parses the input grammar and generates appropriate ParserState pairs.
states :: QuasiQuoter
states =
  QuasiQuoter
    { quoteDec = \(parsepure0 doc . TE.encodeUtf8 . T.pack -> pr) ->
        case pr of
          OK (n, m) _ _ -> do
            let two a b = do p <- a; q <- b; pure [p, q]
            (pure -> t, pure -> b) <- thparserstates m
            two
              do sigD n t
              do valD (varP n) (normalB b) []
          Err e -> error $ "states quasiquoter: unexpected error: " ++ show e
          Fail -> error "states quasiquoter: unexpected error (no message)",
      quoteExp = error "states quasiquoter cannot be used in an expression",
      quotePat = error "states quasiquoter cannot be used in a pattern",
      quoteType = error "states quasiquoter cannot be used in a type"
    }

-- | Represents a pair of parser states - one for server-side parsing and one for client-side.
-- The states contain mappings between packet types, codes and identifiers.
data ParserStates = ParserStates
  { forserver :: ParserState,
    forclient :: ParserState
  }

-- | Internal representation of a single packet definition line from the grammar.
-- Contains the packet name and optional receive/send codes.
data S = S
  { sna :: String, -- packet name
    recv :: Maybe Int, -- receive code (client in/server out)
    send :: Maybe Int -- send code (client out/server in)
  }

-- | Helper for integer conversion
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Lookup with runtime error on missing key
forceindex :: I.IntMap a -> Int -> a
forceindex = (I.!)

-- | List concatenation helper
concat2 :: [a] -> [a] -> [a]
concat2 = (++)

-- | Generate Template Haskell type signature and expression for a ParserStates pair.
-- Takes a list of packet definitions and produces corresponding parser states.
thparserstates :: [S] -> Q (Type, Exp)
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
      -- Server uses send for inbound, recv for outbound
      gensparse = genparse send
      genscode = gencode send recv
      -- Client uses recv for inbound, send for outbound
      gencparse = genparse recv
      genccode = gencode recv send
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
      sig = appT (appT (tupleT 2) (varT 'ParserState)) (varT 'ParserState)
  s1 <- sig
  s2 <- tupE [half gensparse genscode, half gencparse genccode]
  pure (s1, s2)

-- | Parse a single colon character
colon :: Parser st r ()
colon = skipSatisfyAscii (== ':')

-- | Parse a colon with error reporting
colon' :: Parser st r ()
colon' = cut colon "expected colon (:)"

-- | Parse a hexadecimal number into an Int
-- Supports both uppercase and lowercase hex digits
hexnumber :: Parser st r Int
hexnumber = do
  (p, n) <- chainr f digit (pure (1, 0))
  guard (p /= 1)
  pure n
  where
    f n (!place, !a) = (place * 16, a + place * n)
    digit =
      satisfyAscii d <&> \case
        c | '0' <= c && c <= '9' -> ord c - ord '0'
        c | 'A' <= c && c <= 'F' -> ord c - ord 'A'
        c | 'a' <= c && c <= 'f' -> ord c - ord 'a'
        _ -> error "hexnumber/digit: impossible"
    d = liftA2 (||) isDigit (flip (elem @[]) "ABCDEFabcdef")

-- | Parse a hex number with error reporting
hexnumber' :: Parser st r Int
hexnumber' = cut hexnumber "expected a hexadecimal number"

-- | Parse a valid identifier
-- First char must be letter, underscore or quote
-- Later chars can also include dots and digits
ident :: Parser st r String
ident =
  liftA2
    (:)
    do satisfyAscii firstchar
    do many (satisfyAscii laterchar)
  where
    liftany = foldr @[] (liftA2 (||)) (const False)
    firstchar = liftany [(== '\''), (== '_'), isLetter]
    laterchar = liftany [(== '.'), (== '_'), (== '\''), isDigit, isLetter]

-- | Parse an identifier with error reporting
ident' :: Parser st r String
ident' = cut ident "expected an identifier"

-- | Skip a line comment starting with --
linecomment :: Parser st r ()
linecomment =
  withOption
    anyWord8
    (\case 10 -> ws; _ -> linecomment)
    (pure ())

-- | Skip a multi-line comment between {- and -}
-- Handles nested comments correctly
multilinecomment :: Parser st r ()
multilinecomment =
  (1 :: Int) & fix \f -> \case
    0 -> ws
    n ->
      $( switch
           [|
             case _ of
               "-}" -> f (n - 1)
               "{-" -> f (n + 1)
               _ -> branch anyWord8 (f n) (pure ())
             |]
       )

-- | Skip whitespace and comments
ws :: Parser st r ()
ws =
  $( switch
       [|
         case _ of
           " " -> ws
           "\t" -> ws
           "\r" -> ws
           "--" -> linecomment
           "{-" -> multilinecomment
           _ -> pure ()
         |]
   )

-- | Skip to end of current line
skipline :: Parser st r ()
skipline = skipManyTill anyWord8 (eof <|> skipSatisfyAscii (== '\n'))

-- | Parse a single packet definition line
-- Format: <name>:<recv code>:<send code>
line :: Parser st r S
line = do
  sna <- ws *> ident' <* colon'
  recv <- ws *> optional hexnumber' <* colon'
  send <- ws *> optional hexnumber' <* skipline
  pure S {..}

-- | Parse the complete grammar document
-- First line contains state name
-- Remaining lines contain packet definitions
doc :: Parser st r (Name, [S])
doc = do
  -- name of the pair
  n <- mkName <$> ident'
  -- body
  m <- many (many skipline *> line)
  pure (n, m)
