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
import Control.Monad
import Data.ByteString qualified as B
import Data.Char hiding (isDigit)
import Data.Foldable1
import Data.Function
import Data.Functor
import Data.List.NonEmpty qualified as NEL
import Debug.Trace
import FlatParse.Stateful hiding (Parser, Result)
import Language.Haskell.TH qualified as TH
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

-- a variant of the shunting-yard algorithm
-- here the only operator (@) takes exactly two arguments
-- and associates to the left
alwaysleft :: [String] -> [String]
alwaysleft = go [] []
  where
    isop = (== "@")
    islp = (== "(")
    isrp = (== ")")
    go ~out ~ops (t : ts)
      | isop t =
          let (sl, sr) = break islp ops
           in go (reverse sl ++ out) (t : sr) ts
    go ~out ~ops (t : ts) | islp t = go out (t : ops) ts
    go ~out ~ops (t : ts) | isrp t =
      case break islp ops of
        (sl, srh : srt) | islp srh -> go (reverse sl ++ out) srt ts
        _ -> error "ill-formed expression"
    go ~out ~ops [] = reverse out ++ reverse ops
    go ~out (op : ops) (t : ts)
      | not (islp op) && not (isrp op) =
          go (op : t : out) ops ts
    go ~out ops (t : ts) = go (t : out) ops ts

-- uses eof for correctness. use 'isolate'.
thhasktype :: P TH.Type
thhasktype =
  -- only cover: ParensT, TupleT, AppT, AppKindT, ForallT,
  -- VarT, ConT, PromotedT, LitT, ListT.
  -- no functions or other infix operators.
  ws *> choice @[] (map (<* eof) parsers)
  where
    -- caution: if vart were attempted first then appkindt, forallt, etc.
    -- may be subsumed by it, by nature of the grammar. so attempt
    -- these more specific ones first. same for appt vs. contt and others.
    parsers =
      [litt, listt, promotedt, appkindt]
        ++ [tuplet, forallt, appt, cont, vart]
    vart0 = (TH.mkName <$> liftA2 (:) vahead (many chtail)) <* ws
    cont0 = (TH.mkName <$> liftA2 (:) cohead (many chtail)) <* ws
    cont = TH.ConT <$> cont0
    vart = TH.VarT <$> vart0
    appt =
      foldl1' TH.AppT . NEL.fromList
        <$> do
          some do
            choice @[]
              [cont, vart, tuplet, litt, listt, promotedt]
        <* ws
    tuplet =
      let prefix = between lpar rpar (some comma) <&> (TH.TupleT . length)
          regular = between lpar rpar do
            items <- flip sepBy comma do
              choice @[] parsers
            pure case items of
              i : [] -> TH.ParensT i
              _ -> foldl' TH.AppT (TH.TupleT (length items)) items
       in ws *> (prefix <|> regular) <* ws
    appkindt =
      TH.AppKindT <$> (fails atsymb *> self2 <* atsymb <* ws) <*> self3
      where
        self2 = choice @[] [appt, cont, vart, tuplet]
        self3 =
          choice @[]
            [cont, vart, tuplet, promotedt, litt, listt]
    forallt = do
      -- no KindedTV (that is, (a :: k) form) support yet
      ns <- $(string "forall") *> ws *> many vart0
      let vars = [TH.PlainTV name TH.SpecifiedSpec | name <- ns]
      $(char '.') *> ws
      ct <- option [] do
        between lpar rpar (sepBy1 (choice @[] [appt, cont]) comma)
      unless (null ct) do
        ws <* $(string "=>") <* ws
      TH.ForallT vars ct <$> choice @[] do
        [litt, listt, promotedt, appkindt, tuplet, appt, cont, vart]
    promotedt = TH.PromotedT <$> (quote *> cont0) -- data con
    litt = TH.LitT <$> (choice @[] [numtylit, strtylit, chartylit] <* ws)
    listt =
      between llist rlist $
        foldl' TH.AppT TH.ListT
          <$> flip sepBy comma do
            choice @[] $ [litt, listt, promotedt, appkindt]
              ++ [tuplet, forallt, appt, cont, vart]
    lpar = ws *> $(char '(') *> ws
    rpar = ws *> $(char ')') *> ws
    comma = ws *> $(char ',') *> ws
    atsymb = $(char '@')
    quote = $(char '\'')
    dbquote = $(char '"')
    bksp = $(char '\\')
    llist = ws *> $(char '[') *> ws
    rlist = ws *> $(char ']') *> ws
    vahead = satisfy \c -> (isLetter c && isLower c) || c == '_'
    cohead = satisfy \c -> (isLetter c && isUpper c) || c == '_'
    chtail = satisfy \c -> isLetter c || c == '_' || c == '\'' || isDigit c
    numtylit = TH.NumTyLit <$> anyAsciiDecimalInteger
    strtylit = TH.StrTyLit <$> between dbquote dbquote (many escaped)
    chartylit = TH.CharTyLit <$> between quote quote escaped'
    escaped = approved <|> (bksp *> dbquote $> '"')
    escaped' = approved <|> (bksp *> quote $> '\'')
    approved = satisfy isPrint
