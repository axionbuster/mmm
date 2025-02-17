{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedLists #-}

module M.PkMacro
  ( DataDecl (..), -- debug
    FieldType (..), -- debug
    Field (..), -- debug
    P, -- debug
    hasktype, -- debug
    datadecl, -- debug
    pkmacrobody, -- debug
    pkmacro, -- real
    setdefaultderives, -- real
    setproperderives, -- real
    setshadowderives, -- real
  )
where

import Control.Applicative.Combinators hiding (optional, (<|>))
import Control.Category ((>>>))
import Control.Monad
import Data.ByteString qualified as B
import Data.Char hiding (isDigit)
import Data.Coerce
import Data.Foldable1
import Data.Function
import Data.Functor
import Data.List.NonEmpty qualified as NEL
import Data.Maybe
import Data.Typeable
import FlatParse.Stateful hiding (Parser, Result)
import GHC.Generics
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import M.Pack

-- data A {
--  field1 :: Type via Type,
--  field2 :: Type,
--  deriving (Classes 1)
--    and shadow deriving (Pack, Unpack)
--    with (Classes 2)
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
  { dataname :: TH.Name,
    dataflds :: [Field],
    dataderp :: [TH.Type], -- proper
    dataders :: [TH.Type] -- shadow
  }
  deriving (Show)

data Field = Field
  { fieldname :: TH.Name,
    fieldtype :: FieldType
  }
  deriving (Show)

data FieldType = FieldType
  { typemain :: TH.Type,
    typevia :: Maybe TH.Type
  }
  deriving (Show)

data ShadowDir = ViaShadow | ViaMain
  deriving (Show, Eq)

-- used in datadecl
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

ewrap :: Parser st r a -> Parser st r a
ewrap = flip withError \e -> do
  r <- lookahead takeRest
  let r' = if B.length r > le then B.append (B.take le r) "..." else r
      le = 36
  err $ e <> (ParseError $ ", the rest being " ++ show r')

datadecl :: P DataDecl
datadecl = do
  let data' = $(string "data")
      openb = ewrap $ cut $(char '{') "expecting '{'"
      closb = ewrap $ cut $(char '}') "expecting '}'"
      deriv = $(string "deriving")
      comma = ws *> $(char ',') <* ws
      shado = do
        do $(string "and") *> ws *> $(string "shadow") *> ws
        do $(string "deriving") *> ws *> $(char '(') *> ws *> $(string "Pack")
        do comma *> $(string "Unpack") *> ws *> $(char ')') *> ws
        do $(string "with")
      doubc = ewrap $ cut $(string "::") "expecting '::'"
      parsety =
        hasktype >>= do
          strToUtf8 >>> parsepure0 (thhasktype <* eof) >>> \case
            OK v _ _ -> pure v
            Fail -> empty
            Err e -> err e
  dataname <-
    TH.mkName <$> do
      ws *> data' *> ws *> typeident' <* ws <* openb <* ws
  dataflds <- flip endBy comma do
    ws *> fails $(string "deriving") *> do
      fn <- TH.mkName <$> (fieldident <* ws <* doubc <* ws)
      ty <- parsety
      vi <- optional ($(string "via") *> ws *> parsety <* ws)
      pure $ Field fn (FieldType ty vi)
  let parsetypes =
        let t = sepBy parsety comma
         in cut
              ((between $(char '(') $(char ')') t <|> t) <* ws)
              "expecting type; types"
  ws
  dataderp <- option [] do deriv *> ws *> parsetypes
  dataders <- option [] do shado *> ws *> parsetypes
  DataDecl {..} <$ closb <* ws

_tester1 :: Result DataDecl
_tester1 =
  parsepure0
    datadecl
    "data A {\
    \  field1 :: Type via Type @A @i @223 a b (C @i),\
    \  field2 :: Type,\
    \  deriving (B) and shadow deriving (Pack, Unpack) with (D, E)\
    \}"

_tester2 :: Result DataDecl
_tester2 =
  parsepure0
    datadecl
    "data B {\
    \ field1 :: Type,\
    \ deriving (C)\
    \}"

thhasktype :: P TH.Type
thhasktype =
  -- only cover: ParensT, TupleT, AppT, AppKindT, ForallT,
  -- VarT, ConT, PromotedT, LitT, ListT.
  -- no functions or other infix operators.
  ws *> choice parsers
  where
    -- caution: if vart were attempted first then appkindt, forallt, etc.
    -- may be subsumed by it, by nature of the grammar. so attempt
    -- these more specific ones first. same for appt vs. cont and others.
    parsers =
      [litt, listt, promotedt, appkindt]
        ++ [tuplet, forallt, appt, cont, vart]
    vart0 = (TH.mkName <$> liftA2 (:) vahead (many chtail)) <* ws
    cont0 = (TH.mkName <$> liftA2 (:) cohead (many chtail)) <* ws
    cont = TH.ConT <$> cont0
    vart = TH.VarT <$> vart0
    appt =
      foldl1' TH.AppT . NEL.fromList
        <$> some do
          choice @[]
            [cont, vart, tuplet, litt, listt, promotedt]
        <* ws
    tuplet =
      let prefix = between lpar rpar (some comma) <&> (TH.TupleT . length)
          regular = between lpar rpar do
            items <- flip sepBy comma do choice parsers
            pure case items of
              i : [] -> TH.ParensT i
              _ -> foldl' TH.AppT (TH.TupleT (length items)) items
       in ws *> (prefix <|> regular) <* ws
    appkindt = do
      base <- choice [cont, vart, tuplet]
      foldl' (flip ($)) base <$> many do
        let next = choice [litt, listt, promotedt, tuplet, cont, vart]
        choice
          [ (flip TH.AppKindT <$> (atsymb *> next)),
            (flip TH.AppT <$> next)
          ]
    forallt = do
      -- no KindedTV (that is, (a :: k) form) support yet
      ns <- $(string "forall") *> ws *> many vart0
      let vars = [TH.PlainTV name TH.SpecifiedSpec | name <- ns]
      $(char '.') *> ws
      ct <- option [] do
        between lpar rpar (sepBy1 (choice [appt, cont]) comma)
      unless (null ct) do
        ws <* $(string "=>") <* ws
      TH.ForallT vars ct <$> choice do
        [litt, listt, promotedt, appkindt, tuplet, appt, cont, vart]
    promotedt = TH.PromotedT <$> (quote *> cont0) -- data con
    litt = TH.LitT <$> (choice [numtylit, strtylit, chartylit] <* ws)
    listt =
      between llist rlist $
        foldl' TH.AppT TH.ListT
          <$> flip sepBy comma do
            choice $
              [litt, listt, promotedt, appkindt]
                ++ [tuplet, forallt, appt, cont, vart]
    lpar = ws *> $(char '(') *> ws
    rpar = ws *> $(char ')') *> ws
    comma = ws *> $(char ',') *> ws
    atsymb = ws *> $(char '@') <* ws
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

infixr 3 #

(#) :: TH.Q a -> TH.Q [a] -> TH.Q [a]
(#) = liftA2 (:)

data PkState = PkState
  { pkderp :: [TH.Type],
    pkders :: [TH.Type]
  }
  deriving (Show, Typeable)

-- user-facing

setdefaultderives :: TH.Q [TH.Dec]
setdefaultderives = do
  pkinit
  void $ setproperderives [''Generic]
  void $ setshadowderives [''Generic, ''Pack, ''Unpack]
  pure []

setproperderives :: [TH.Name] -> TH.Q [TH.Dec]
setproperderives a = do
  pkinit
  pkmodify \b -> b {pkderp = TH.ConT <$> a}
  pure []

setshadowderives :: [TH.Name] -> TH.Q [TH.Dec]
setshadowderives a = do
  pkinit
  pkmodify \b -> b {pkders = TH.ConT <$> a}
  pure []

pkinit :: TH.Q ()
pkinit =
  TH.getQ >>= \case
    Just (_ :: PkState) -> noop
    Nothing -> pkput $ PkState [] []

pkput :: PkState -> TH.Q ()
pkput = TH.putQ

pkget :: TH.Q PkState
pkget =
  TH.getQ <&> \case
    Just s -> s
    Nothing -> error "pkget failed"

pkmodify :: (PkState -> PkState) -> TH.Q ()
pkmodify f = do
  a <- pkget
  pkput (f a)

pkmacrobody :: [DataDecl] -> TH.Q [TH.Dec]
pkmacrobody decls = do
  pkinit
  pkstate <- pkget
  join <$> do
    forM decls \decl -> do
      let ders = pkstate.pkders ++ decl.dataders
          derp = pkstate.pkderp ++ decl.dataderp
          nobang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
          maindecl =
            TH.dataD
              (pure []) -- Cxt
              decl.dataname
              [] -- no type variables
              Nothing -- no exotic kind
              [ TH.recC
                  decl.dataname
                  [ pure (f.fieldname, nobang, f.fieldtype.typemain)
                  | f <- decl.dataflds
                  ]
              ]
              [ TH.derivClause
                  Nothing
                  (fmap pure derp)
              ]
          mkshadow (TH.Name (TH.OccName n) nf) =
            TH.Name (TH.OccName (n ++ "__")) nf
          shadowdecl =
            TH.dataD
              (pure []) -- no Cxt
              (mkshadow decl.dataname)
              []
              Nothing
              [ TH.recC
                  (mkshadow decl.dataname)
                  ( [ pure $
                        maybe
                          (n, nobang, f.fieldtype.typemain)
                          (\v -> (n, nobang, v))
                          f.fieldtype.typevia
                    | f <- decl.dataflds,
                      let n = mkshadow f.fieldname
                    ]
                  )
              ]
              [ TH.derivClause
                  Nothing
                  (fmap pure ders)
              ]
          bridgepack = do
            self <- TH.newName "self"
            TH.instanceD
              (pure []) -- no Cxt
              (TH.appT (TH.conT ''Pack) (TH.conT decl.dataname))
              [ TH.funD
                  'pack
                  [ [] & TH.clause [TH.varP self] do
                      TH.normalB do
                        TH.appE (TH.varE 'pack) do
                          TH.recConE
                            (mkshadow decl.dataname)
                            [ (mkshadow f.fieldname,)
                                <$> TH.appE (TH.varE 'coerce) do
                                  TH.appE (TH.varE f.fieldname) (TH.varE self)
                            | f <- decl.dataflds
                            ]
                  ]
              ]
          bridgeunpack = do
            other <- TH.newName "other"
            TH.instanceD
              (pure []) -- no Cxt
              (TH.appT (TH.conT ''Unpack) (TH.conT decl.dataname))
              [ [] & TH.valD (TH.varP 'unpack) do
                  TH.normalB do
                    TH.doE
                      [ TH.bindS (TH.varP other) (TH.varE 'unpack),
                        TH.noBindS do
                          TH.appE (TH.varE 'pure) do
                            TH.recConE
                              decl.dataname
                              [ (f.fieldname,)
                                  <$> TH.appE (TH.varE 'coerce) do
                                    TH.appE
                                      (TH.varE (mkshadow f.fieldname))
                                      (TH.varE other)
                              | f <- decl.dataflds
                              ]
                      ]
              ]
      maindecl
        # if any (isJust . typevia . fieldtype) decl.dataflds
          then sequence [shadowdecl, bridgepack, bridgeunpack]
          else pure []

punwrap :: Result a -> a
punwrap (OK v _ _) = v
punwrap Fail = error "unexpected uninformative failure"
punwrap (Err e) = error ("parsing error: " ++ showparseerror e)

pkmacro :: TH.QuasiQuoter
pkmacro =
  TH.QuasiQuoter
    { quoteExp = error "pkmacro is not an expression quoter",
      quotePat = error "pkmacro is not a pattern quoter",
      quoteType = error "pkmacro is not a type quoter",
      quoteDec =
        pkmacrobody
          . punwrap
          . parsepure0 (ewrap (cut (many datadecl <* eof) "pkmacro parse"))
          . strToUtf8
    }
