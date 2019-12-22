{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mixfix.Parse
    ( module Mixfix.Parse
    ) where

import qualified Control.Applicative        (some)
import           Control.Monad.Reader
import           Data.Bifunctor             (first)
import           Data.Data                  (Data)
import           Data.Either
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.List                  (foldr1, intersperse)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Typeable              (Typeable)
import           Data.Void                  (Void)
import           Debug.Trace
import           GHC.Generics               (Generic)
import qualified Text.Earley                as Earley
import qualified Text.Earley.Mixfix         as Earley
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Mixfix.Syntax

---

type PrecTable = IntMap [(Earley.Holey Text, Associativity)]

type Parser = ParsecT Void Text (Reader PrecTable)

sc :: Parser ()
sc = L.space space1 lineComment blockComment
    where
        blockComment = empty
        lineComment = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

intLiteral :: Parser Int
intLiteral = label "int" $ lexeme L.decimal

floatLiteral :: Parser Double
floatLiteral = label "float" $ lexeme L.float

stringLiteral :: Parser Text
stringLiteral = label "string" $ lexeme $ fmap Text.pack $ char '"' >> L.charLiteral `manyTill` char '"'

charLiteral :: Parser Char
charLiteral = label "char" $ lexeme $ between (char '\'') (char '\'') L.charLiteral

identifier :: Parser Text
identifier = label "identifier" $ lexeme $ try opIdent <|> try letterIdent
    where
        letterIdent :: Parser Text
        letterIdent = do
            i <- fmap Text.pack $ (:) <$> (lowerChar <|> char '_') <*> many (alphaNumChar <|> char '_')
            if i `Set.member` reservedWords
               then fail $ Text.unpack i ++ " is a reserved word"
               else pure i

        opIdent :: Parser Text
        opIdent = do
            i <- Text.pack <$> some (oneOf ("=+*<>$%^&[]!|-:/_," :: [Char]))
            if i `Set.member` reservedOps
               then fail $ Text.unpack i ++ " is a reserved operator"
               else pure i

        reservedWords :: Set Text
        reservedWords = Set.fromList ["let", "in", "assocr", "assocl", "nonassoc"]

        reservedOps :: Set Text
        reservedOps = Set.fromList ["="]

---

parseExp :: Parser Exp
parseExp = some (try parseExpAtom) >>= \case
    [x] -> pure (either Var id x)
    xs  -> distinguish xs

parseExpAtom :: Parser Tok
parseExpAtom =
        try (Right <$> parseLam)
    <|> try (Right <$> parseLet)
    <|> try (Right <$> parseLetAssoc)
    <|> try (Right . Lit <$> parseLit)
    <|> try (Right <$> parseParen)
    <|>     (Left  <$> identifier)
    where
        parseLet = do
            symbol "let"
            x <- identifier
            symbol "="
            e <- parseExp
            symbol "in"
            Let x e <$> parseExp
        parseAssoc =
                try (AssocLeft  <$ symbol "assocl")
            <|> try (AssocRight <$ symbol "assocr")
            <|>     (AssocNone  <$ symbol "nonassoc")
        parseLetAssoc = do
            symbol "let"
            a <- parseAssoc
            n <- intLiteral
            x <- identifier
            symbol "in"
            let f :: IntMap [([Maybe Text], Associativity)] -> IntMap [([Maybe Text], Associativity)]
                f = IntMap.alter (let e = (toHole x, a) in Just . maybe [e] (e:)) n
            LetAssoc a n x <$> local f parseExp
        parseLam = do
            symbol "\\"
            x <- identifier
            symbol "->"
            Lam x <$> parseExp
        parseParen = between (symbol "(") (symbol ")") parseExp

        toHole :: Text -> Earley.Holey Text
        toHole txt =
            case Text.uncons txt of
              Nothing        -> []
              Just ('_', xs) -> Nothing : toHole xs
              Just _         -> Just i : toHole rest
                  where (i, rest) = Text.span (/= '_') txt

parseLit :: Parser Literal
parseLit =
        try (LitInt   <$> intLiteral)
    <|> try (LitStr   <$> stringLiteral)
    <|> try (LitChar  <$> charLiteral)
    <|>     (LitFloat <$> floatLiteral)

---

-- | Represents a token passed to the Earley parser
-- Text - an identifier. May be part of a mixfix operator
-- Exp - an expression. Will not be part of a mixfix operator
type Tok = Either Text Exp

getIdentTable :: Parser [[(Earley.Holey Text, Earley.Associativity)]]
getIdentTable =
    let assoc :: Associativity -> Earley.Associativity
        assoc AssocRight = Earley.RightAssoc
        assoc AssocLeft  = Earley.LeftAssoc
        assoc AssocNone  = Earley.NonAssoc

        transformPrecTable :: PrecTable -> [[(Earley.Holey Text, Earley.Associativity)]]
        transformPrecTable intmap = map (map (fmap assoc)) (snd <$> IntMap.toAscList intmap)

     in asks transformPrecTable

distinguish :: [Tok] -> Parser Exp
distinguish tokens = do
    env <- getIdentTable
    traceM $ "distinguishing between: " ++ show tokens
    traceM $ "env: " ++ show env
    t <- getIdentTable
    case p t tokens of
      ([], r)  -> fail $ "Parsing error: " ++ show r
      (x:_, _) -> pure x

type Prod r = Earley.Prod r Text Tok
type Grammar = Earley.Grammar

p :: [[(Earley.Holey Text, Earley.Associativity)]]
  -> [Tok]
  -> ([Exp], Earley.Report Text [Tok])
p tbl = Earley.fullParses (Earley.parser (grammar tbl))

grammar
    :: forall r
    . [[(Earley.Holey Text, Earley.Associativity)]]
    -> Grammar r (Prod r Exp)
grammar tbl = mdo
    let app :: Earley.Holey Text -> [Tok] -> Tok
        app hole toks = Right $ foldl1 App (Var (holeToName hole) : fmap convert toks)
        holeToName :: Earley.Holey Text -> Text
        holeToName []             = ""
        holeToName (Nothing:rest) = "_" <> holeToName rest
        holeToName (Just x:rest)  = x <> holeToName rest
        nt :: Text -> Prod r Text
        nt t = Earley.terminal $ \case
            Left x | x == t -> Just x
            _ -> Nothing
        tbl' :: [[(Earley.Holey (Prod r Text), Earley.Associativity)]]
        tbl' = map (map $ first $ map $ fmap nt) tbl
        convert :: Tok -> Exp
        convert = either Var id
        f :: Tok -> [Tok] -> Tok
        f t ts = Right $ foldl1 App (convert t : fmap convert ts)
        reserved :: Set Text
        reserved = Set.fromList $ catMaybes $ concatMap fst $ concat tbl
        isReserved :: Tok -> Bool
        isReserved (Left x) | x `Set.member` reserved = True
        isReserved _ = False
    atom      <- Earley.rule $ Earley.satisfy (not . isReserved)                     :: Grammar r (Prod r Tok)
    normalApp <- Earley.rule $ atom <|> f <$> atom <*> Control.Applicative.some atom :: Grammar r (Prod r Tok)
    expr      <- Earley.mixfixExpression tbl' normalApp app                          :: Grammar r (Prod r Tok)
    pure $ fmap convert expr
