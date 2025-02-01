{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ast.AstSrc (AstSrc (AstSrc)) where

import Ast.Ast (IsAst (underlay))
import Error.Diagnose (Position)
import Source (HasPosition, pos)

data AstSrc a = AstSrc Position a

instance (Show a) => Show (AstSrc a) where
    show :: AstSrc a -> String
    show (AstSrc _ x) = show x

instance IsAst (AstSrc a) a where
    underlay :: AstSrc a -> a
    underlay (AstSrc _ x) = x

instance (Eq a) => Eq (AstSrc a) where
    (==) :: AstSrc a -> AstSrc a -> Bool
    (AstSrc _ x) == (AstSrc _ y) = x == y

instance Functor AstSrc where
    fmap :: (a -> b) -> AstSrc a -> AstSrc b
    fmap f (AstSrc sp x) = AstSrc sp $ f x

instance HasPosition (AstSrc a) where
    pos :: AstSrc a -> Position
    pos (AstSrc sp _) = sp
