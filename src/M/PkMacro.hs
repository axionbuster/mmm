{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedLists #-}

-- |
-- Module: M.PkMacro
-- Description: Template Haskell generator for data types with 'Pack'\/'Unpack' instances
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- This module provides Template Haskell functionality to generate data types with
-- automatic 'Pack'\/'Unpack' instances. It uses a simple grammar to define data types
-- and their field mappings.
--
-- == Usage
--
-- Define data types using the 'pkmacro' quasi-quoter. The syntax is indentation-insensitive:
--
-- @
-- -- First, set up default instances
-- 'setdefaultderives'  -- Sets up 'Generic', 'Pack', and 'Unpack' derives
--
-- -- Define a newtype wrapper with 'Pack'\/'Unpack' instances
-- newtype AAA = AAA 'Data.Int.Int32'
--   deriving stock ('Generic')
--   deriving newtype ('Pack', 'Unpack')
--
-- [pkmacro|
-- -- Regular data type with two fields
-- data A {
--   f1 :: Int32,                -- Regular field
--   f2 :: Int32 via AAA,       -- Field with custom serialization
-- }
--
-- -- Data type with one field and explicit deriving
-- data B {
--   f3 :: Int32,
--   deriving ('Generic', 'Show')
-- }
--
-- -- Empty data type (creates constructor with no fields)
-- data C {}
-- |]
-- @
--
-- The grammar supports:
--
-- * Empty data types (no fields)
-- * Custom serialization via @via@ clause
-- * Multiple data types in one block
-- * Indentation-insensitive syntax
-- * Comments (both @--@ and @{- -}@ style)
--
-- == Syntax
--
-- The full syntax for data type definitions is:
--
-- @
-- data TypeName {
--   field1 :: Type1 [via Type2],     -- Field with optional via clause
--   field2 :: Type3,                 -- Regular field
--   [deriving (Class1, Class2)]      -- Optional proper deriving clause
--   [and shadow deriving ('Pack', 'Unpack') with (Class3, Class4)] -- Optional shadow deriving
-- }
-- @
--
-- Elements in square brackets are optional.
--
-- * The @via@ clause specifies a different type to use for serialization
-- * @deriving@ adds instances to the main data type
-- * @shadow deriving@ adds instances to the generated shadow type used for serialization
-- * Multiple data types can be defined in a single quasi-quoter block
--
-- === Type Syntax
--
-- Types can include:
--
-- * Simple types: @'Data.Int.Int32'@, @'Data.Text.Text'@, etc.
-- * Parameterized types: @'Maybe' a@, @['Int']@
-- * Type applications: @a \@k@
-- * Promoted types: @\'True@, @\'Just@
-- * Type literals: @\"hello\"@, @123@
-- * Parenthesized types: @(a, b)@, @('Either' a b)@
--
-- See: 'Pack', 'Unpack', 'setdefaultderives', 'addproperderives', and 'addshadowderives'.
module M.PkMacro
  ( setdefaultderives,
    addproperderives,
    addshadowderives,
    pkmacro,
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
import Data.List ((\\))
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
    cht c = isLetter c || isDigit c || c == '_' || c == '\'' || c == '.'
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
    chtail = satisfy \c ->
      isLetter c
        || c == '_'
        || c == '\''
        || c == '.'
        || isDigit c
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

-- | Set up default derives for subsequent data types.
-- This sets 'Generic' for proper derives and 'Generic' + 'Pack' + 'Unpack' for shadow derives.
--
-- Use this at the start of your module to automatically derive the most common instances.
setdefaultderives :: TH.Q [TH.Dec]
setdefaultderives = do
  pkinit
  pkmodify \b -> b {pkderp = TH.ConT <$> [''Generic]}
  pkmodify \b -> b {pkders = TH.ConT <$> [''Generic, ''Pack, ''Unpack]}
  pure []

-- | Add proper deriving clauses for subsequent data types.
-- These instances will be derived directly on the main data type.
--
-- @
-- 'addproperderives' [''Generic, ''Show]  -- Derive Generic and Show
-- @
addproperderives :: [TH.Name] -> TH.Q [TH.Dec]
addproperderives names = do
  pkinit
  pkmodify \b -> b {pkderp = b.pkderp ++ map TH.ConT names}
  pure []

-- | Add shadow deriving clauses for subsequent data types.
-- These instances will be derived on the shadow data type used for serialization.
--
-- @
-- 'addshadowderives' [''Generic, ''Pack]  -- Derive 'Generic' and 'Pack' on shadow type
-- @
addshadowderives :: [TH.Name] -> TH.Q [TH.Dec]
addshadowderives names = do
  pkinit
  pkmodify \b -> b {pkders = b.pkders ++ map TH.ConT names}
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
          shadowed = any (isJust . typevia . fieldtype) decl.dataflds
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
              ( [ TH.derivClause
                    Nothing
                    (fmap pure derp)
                ]
                  ++ if shadowed
                    then
                      []
                    else
                      [ TH.derivClause
                          Nothing
                          (fmap pure (ders \\ derp))
                      ]
              )
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
                        names <- forM decl.dataflds (const (TH.newName "a"))
                        TH.letE
                          [ TH.valD
                              (TH.conP decl.dataname (TH.varP <$> names))
                              (TH.normalB (TH.varE self))
                              [] -- no where-bindings
                          ]
                          ( TH.appE (TH.varE 'pack) do
                              TH.appsE do
                                TH.conE (mkshadow decl.dataname)
                                  : [ TH.appE (TH.varE 'coerce) (TH.varE n)
                                    | n <- names
                                    ]
                          )
                  ]
              ]
          bridgeunpack = do
            other <- TH.newName "other"
            TH.instanceD
              (pure []) -- no Cxt
              (TH.appT (TH.conT ''Unpack) (TH.conT decl.dataname))
              [ [] & TH.valD (TH.varP 'unpack) do
                  TH.normalB do
                    names <- forM decl.dataflds (const (TH.newName "a"))
                    TH.doE
                      [ TH.bindS (TH.varP other) (TH.varE 'unpack),
                        TH.letS
                          [ TH.valD
                              (TH.conP (mkshadow decl.dataname) (TH.varP <$> names))
                              (TH.normalB (TH.varE other))
                              []
                          ],
                        TH.noBindS do
                          TH.appE (TH.varE 'pure) do
                            TH.appsE do
                              TH.conE decl.dataname
                                : [ TH.appE (TH.varE 'coerce) (TH.varE n)
                                  | n <- names
                                  ]
                      ]
              ]
      maindecl
        # if shadowed
          then sequence [shadowdecl, bridgepack, bridgeunpack]
          else pure []

punwrap :: Result a -> a
punwrap (OK v _ _) = v
punwrap Fail = error "unexpected uninformative failure"
punwrap (Err e) = error ("parsing error: " ++ showparseerror e)

-- | See module docs ("M.PkMacro") for information.
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
