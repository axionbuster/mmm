{-# LANGUAGE NoMonomorphismRestriction #-}

module M.PkMacro
  ( DataDecl (..),
    FieldType (..),
    Field (..),
    P, -- debug
    hasktype, -- debug
    datadecl,
  )
where

import Control.Applicative.Combinators hiding (optional, (<|>))
import Control.Monad (guard)
import Data.ByteString qualified as B
import Data.Char hiding (isDigit)
import Data.Function
import FlatParse.Stateful hiding (Parser, Result)
import M.Pack

-- data A {
--  field1 :: Type via Type,
--  field2 :: Type,
--  deriving (Classes3)
--    based on (Classes 1)
--    shadow deriving (Classes 2)
-- }

type P a = forall st r. Parser st r a

noop :: (Applicative m) => m ()
noop = pure ()

-- | Skip whitespace and comments
ws :: P ()
ws =
  $( switch
       [|
         case _ of
           " " -> ws
           "\n" -> ws
           "\t" -> ws
           "\r" -> ws
           "--" -> linecomment
           "{-" -> multilinecomment
           _ -> noop
         |]
   )

typeident, typeident' :: P String
typeident = liftA2 (:) (goh chh) (got cht)
  where
    goh = satisfyAscii
    got = many . satisfyAscii
    chh c = (isLetter c && isUpper c) || (c == '_')
    cht c = isLetter c || isDigit c || c == '_' || c == '\''
typeident' = cut typeident "expected a type identifier"

fieldident :: P String
fieldident = liftA2 (:) (goh chh) (got cht)
  where
    goh = satisfyAscii
    got = many . satisfyAscii
    chh c = (isLetter c && isLower c) || (c == '_')
    cht c = isLetter c || isDigit c || c == '_' || c == '\''

linecomment :: P ()
linecomment = withOption anyWord8 (\case 10 -> ws; _ -> linecomment) noop

multilinecomment :: P ()
multilinecomment =
  (1 :: Int) & fix \f -> \case
    0 -> ws
    n ->
      $( switch
           [|
             case _ of
               "-}" -> f (n - 1)
               "{-" -> f (n + 1)
               _ -> branch anyWord8 (f n) noop
             |]
       )

data DataDecl = DataDecl
  { dataname :: String,
    dataflds :: [Field],
    dataderp :: [String], -- proper
    dataders :: [String], -- shadow
    dataderx :: [String] -- proxy
  }
  deriving (Show)

data FieldType = FieldType
  { typename :: String,
    typevia :: Maybe String
  }
  deriving (Show)

data Field = Field
  { fieldname :: String,
    fieldtype :: FieldType
  }
  deriving (Show)

hasktype :: P String
hasktype = ws *> (typeexpr <* ws)
  where
    typeexpr = do
      unwords <$> do
        (:)
          <$> (ws *> typecomponent False <* ws)
          <*> many (typecomponent True <* ws)
    surround c d = (pure c ++) . (++ pure d)
    typecomponent tailpos = do
      let bracketed c d c' d' p = c *> ws *> fmap (surround c' d') p <* d
      lookahead (fails $(string "via")) *> do
        typeident
          <|> (guard tailpos *> fieldident)
          <|> ( $(char '@')
                  *> fmap
                    ('@' :)
                    ( fmap show anyAsciiDecimalInteger
                        <|> typeident
                        <|> fieldident
                        <|> bracketed $(char '(') $(char ')') '(' ')' hasktype
                        <|> bracketed $(char '[') $(char ']') '[' ']' hasktype
                    )
              )
          <|> bracketed $(char '(') $(char ')') '(' ')' typeexpr
          <|> bracketed $(char '[') $(char ']') '[' ']' typeexpr

datadecl :: P DataDecl
datadecl = do
  let data' = ewrap $ cut $(string "data") "expecting 'data'"
      openb = ewrap $ cut $(char '{') "expecting '{'"
      closb = ewrap $ cut $(char '}') "expecting '}'"
      deriv = ewrap $ cut $(string "deriving") "expecting 'deriving'"
      based =
        ewrap $
          cut
            ($(string "based") *> ws *> $(string "on"))
            "expecting 'based on'"
      shado =
        ewrap $
          cut
            ($(string "shadow") *> ws *> $(string "deriving"))
            "expecting 'shadow deriving'"
      doubc = ewrap $ cut $(string "::") "expecting '::'"
      ewrap = flip withError \e -> do
        r <- lookahead takeRest
        let r' = if B.length r > le then B.append (B.take le r) "..." else r
            co (ParseError e1) (ParseError e2) = ParseError (e1 ++ e2)
            le = 36
        err $ e `co` (ParseError $ ", the rest being " ++ show r')
      fieldident' = ewrap $ cut fieldident "expected a field identifier"
  dataname <- ws *> data' *> ws *> typeident' <* ws <* openb
  dataflds <-
    ws
      *> flip endBy $(char ',') do
        ws *> fails $(string "deriving") *> do
          fn <- fieldident' <* ws <* doubc <* ws
          tn <- hasktype <* ws
          vi <- optional ($(string "via") *> ws *> hasktype <* ws)
          pure $ Field fn (FieldType tn vi)
  let parsetypes =
        let t = sepBy hasktype $(char ',')
         in cut
              ((between $(char '(') $(char ')') t <|> t) <* ws)
              "expecting type; types"
  ws
  dataderx <- deriv *> ws *> parsetypes
  dataderp <- based *> ws *> parsetypes
  dataders <- shado *> ws *> parsetypes
  DataDecl {..} <$ closb <* ws

tester1 :: Result DataDecl
tester1 =
  parsepure0
    datadecl
    "data A {\
    \  field1 :: Type via Type @A @i @223 a b (C @i),\
    \  field2 :: Type,\
    \  deriving (B) based on (C, D,E @1 @i,F) shadow deriving ()\
    \}"
