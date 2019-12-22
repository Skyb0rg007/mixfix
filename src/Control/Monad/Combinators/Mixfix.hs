{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Combinators.Mixfix where


import           Control.Monad             (MonadPlus)
import           Control.Monad.Combinators (choice, count)
import           Data.Data                 (Data)
import           Data.Either               (lefts)
import           Data.Foldable             (foldrM)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)

-- | How the operator associates
data Associativity
    = LeftAssoc
    | NonAssoc
    | RightAssoc
    deriving (Eq, Show, Ord, Enum, Generic, Data, Typeable)

-- | if_then_else_ <=> [Just "if", Nothing, Just "then", Nothing, Just "else", Nothing]
type Holey a = [Maybe a]

data Operator m a where
    Op :: Holey (m b)           -- ^ How to parse each section, and where the holes are
       -> Associativity         -- ^ How to associate the operator, if not closed
       -> (Holey b -> [a] -> a) -- ^ How to assemble the operator after parsing
       -> Operator m a

---

-- | Group consecutive holes and ident pairs
-- A grp-ed list contains alterating Left-Right elements
grp :: Applicative f => [Maybe (f a)] -> [Either Int (f [a])]
grp [] = []
grp (Nothing : ps) = case grp ps of
                       Left n : rest -> (Left $! n + 1) : rest
                       rest          -> Left 1 : rest
grp (Just p : ps) = case grp ps of
                      Right ps' : rest -> Right ((:) <$> p <*> ps') : rest
                      rest -> Right ((:) <$> p <*> pure []) : rest

makeExprParser
    :: MonadPlus m
    => m a -- ^ term parser
    -> [[Operator m a]] -- ^ Precedence table
    -> m a -- ^ resulting expression parser
makeExprParser = foldl addPrecLevel

addPrecLevel :: MonadPlus m => m a -> [Operator m a] -> m a
addPrecLevel = undefined

-- addPrecLevel
    -- :: forall m ident expr . MonadPlus m
    -- => m expr -- ^ current parser
    -- -> [(Holey (m ident), Associativity, Holey ident -> [expr] -> expr)] -- ^ ops to add
    -- -> m expr
-- addPrecLevel term ops =
    -- choice (map mixfixIdent ops)
    -- where
        -- expr = undefined
        -- next = undefined
        -- same = undefined

        -- mixfixIdent
            -- :: (Holey (m ident), Associativity, Holey ident -> [expr] -> expr)
            -- -> m expr
        -- mixfixIdent (ps, a, f) = f' <$> go (grp ps)
            -- where
                -- f' :: [Either [expr] [ident]] -> expr
                -- f' xs = f
                        -- (concatMap (either (map $ const Nothing) (map Just)) xs)
                        -- (concat $ lefts xs)
                -- go :: [Either Int (m [ident])] -> m [Either [expr] [ident]]
                -- go = \case
                    -- [] -> pure []
                    -- [Right p] -> pure . Right <$> p
                    -- Left n : rest -> (:) <$> (Left <$> count n (if a == RightAssoc then next else same)) <*> go rest
                    -- [Right p, Left n] -> (:) <$> (Right <$> p) <*> (pure . Left <$> count n (if a == LeftAssoc then next else same))
                    -- Right p : Left n : rest -> (:) <$> (Right <$> p) <*> ((:) <$> (Left <$> count n expr) <*> go rest)
                    -- Right _ : Right _ : _ -> error "Not possible"

