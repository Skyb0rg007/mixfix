{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- {-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Mixfix.Syntax
    ( module Mixfix.Syntax
    ) where

import           Data.Data                             (Data)
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List.NonEmpty                    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                    as NonEmpty
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)

data Exp
    = App !Exp !Exp                           -- e₁ e₂
    | Lam !Text !Exp                          -- λx → e
    | Var !Text                               -- x
    | Let !Text !Exp !Exp                     -- let x = e₁ in e₂
    | LetAssoc !Associativity !Int !Text !Exp -- let assoc[lr] if_then_else_ in e
    | Lit !Literal                            -- 1 | "asdf" | 'c'
    | Object !(Map Text Exp)                  -- { lbl₁ = e₁, lbl₂ = e₂ }
    | ObjGet !Exp !Text                       -- obj.x
    deriving (Show, Eq, Ord, Generic, Data, Typeable)

data Associativity
    = AssocLeft  -- assocl
    | AssocNone  -- nonassoc
    | AssocRight -- assocr
    deriving (Show, Eq, Ord, Enum, Generic, Data, Typeable)

data Literal
    = LitInt   !Int    -- 1
    | LitStr   !Text   -- "asdf"
    | LitChar  !Char   -- 'c'
    | LitFloat !Double -- 2.0
    deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- makeBaseFunctor ''Exp

instance Pretty Associativity where
    pretty = \case
        AssocLeft  -> "assocl"
        AssocNone  -> "nonassoc"
        AssocRight -> "assocr"

instance Pretty Literal where
    pretty = \case
        LitInt n   -> pretty n
        LitStr s   -> "\"" <> pretty s <> "\""
        LitChar c  -> "'" <> pretty c <> "'"
        LitFloat f -> pretty f <> "f"

instance Pretty Exp where
    pretty = prettyPrec 0
        where
            parenIf :: Bool -> Doc ann -> Doc ann
            parenIf True  = parens
            parenIf False = id

            prettyField :: (Text, Exp) -> Doc ann
            prettyField (lbl, e) = pretty lbl <+> "=" <+> prettyPrec 0 e

            prettyPrec :: Int -> Exp -> Doc ann
            prettyPrec p = \case
                App e1 e2 -> parenIf (p > 1) $
                    prettyPrec 1 e1 <+> prettyPrec 2 e2
                Lam x e -> parenIf (p > 0) $
                    "λ" <> pretty x <+> "→" <+> prettyPrec 0 e
                Var x -> pretty x
                Let x e1 e2 -> parenIf (p > 0) $
                    "let" <+> pretty x <+> "=" <+> prettyPrec 0 e1 <+> "in" <+> prettyPrec 0 e2
                LetAssoc a n x e -> parenIf (p > 0) $
                    "let" <+> pretty a <+> pretty n <+> pretty x <+> "in" <+> prettyPrec 0 e
                Lit l -> pretty l
                Object o -> encloseSep "{" "}" "," (prettyField <$> Map.toList o)
                ObjGet e x -> prettyPrec 2 e <> "." <> pretty x

