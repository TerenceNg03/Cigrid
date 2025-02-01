{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Ast.Ast (Phase (..), Ast, IsAst (..)) where

data Phase = Lexed | Parsed | Named | Typed

type family Ast (p :: Phase) a

class IsAst a b where
    underlay :: a -> b

instance (IsAst a b, Functor f) => IsAst (f a) (f b) where
    underlay :: f a -> f b
    underlay = fmap underlay
