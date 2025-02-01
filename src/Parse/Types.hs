{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Types (ParseM (..), Context (..), runParse) where

import Ast.Ast (Ast, Phase (Lexed, Parsed))
import Ast.AstSrc (AstSrc)
import Ast.Token (Token (..))
import Data.Bifunctor (first)
import Data.Text (Text)
import Error.Diagnose (Position, Report)
import Source (Source)

type instance Ast Parsed a = AstSrc a

data Context = Context
    { source :: Source
    , tokens :: [Ast Lexed Token]
    }

newtype ParseM a = ParseM {runParseM :: Context -> Either (Position, Report Text) (a, Context)}

instance Functor ParseM where
    fmap :: (a -> b) -> ParseM a -> ParseM b
    fmap f m = ParseM $ fmap (first f) . runParseM m

instance Applicative ParseM where
    pure :: a -> ParseM a
    pure x = ParseM $ \c -> Right (x, c)
    (<*>) :: ParseM (a -> b) -> ParseM a -> ParseM b
    m1 <*> m2 = do
        f <- m1
        f <$> m2

instance Monad ParseM where
    (>>=) :: ParseM a -> (a -> ParseM b) -> ParseM b
    m >>= f = ParseM $ \c -> do
        (a, c') <- runParseM m c
        runParseM (f a) c'

runParse :: ParseM a -> Source -> [Ast Lexed Token] -> Either (Position, Report Text) a
runParse m src tks =
    let context = Context src tks
     in fst <$> runParseM m context
