{-# LANGUAGE OverloadedLists #-}

module Operator (prefixPre, infixPre, maybePrefix, maybeInfix) where

import Ast.BOp (BOp (..))
import qualified Ast.BOp as Ast
import Ast.Token (Token (..))
import qualified Ast.Token as Token
import Ast.UOp (UOp (..))
import qualified Ast.UOp as Ast
import Data.Map (Map)

maybePrefix :: Token -> Maybe UOp
maybePrefix Token.Not = Just Ast.Not
maybePrefix Token.BitNot = Just Ast.BitNot
maybePrefix Minus = Just MinusU
maybePrefix _ = Nothing

maybeInfix :: Token -> Maybe BOp
maybeInfix Token.Plus = Just Ast.Plus
maybeInfix Token.Minus = Just Ast.MinusB
maybeInfix Token.Mul = Just Ast.Mul
maybeInfix Token.Div = Just Ast.Div
maybeInfix Token.Mod = Just Ast.Mod
maybeInfix Token.Lt = Just Ast.Lt
maybeInfix Token.Le = Just Ast.Le
maybeInfix Token.Ge = Just Ast.Ge
maybeInfix Token.Gt = Just Ast.Gt
maybeInfix Token.Eq = Just Ast.Eq
maybeInfix Token.Ne = Just Ast.Ne
maybeInfix Token.And = Just Ast.And
maybeInfix Token.Or = Just Ast.Or
maybeInfix Token.BitAnd = Just Ast.BitAnd
maybeInfix Token.BitOr = Just Ast.BitOr
maybeInfix Token.LeftShift = Just Ast.LeftShift
maybeInfix Token.RightShift = Just Ast.RightShift
maybeInfix _ = Nothing

prefixPre :: Map UOp ((), Int)
prefixPre =
    [ (Ast.Not, ((), 21))
    , (Ast.BitNot, ((), 21))
    , (MinusU, ((), 21))
    ]

infixPre :: Map BOp (Int, Int)
infixPre =
    [ (Ast.Or, (1, 2))
    , (Ast.And, (3, 4))
    , (Ast.BitOr, (5, 6))
    , (Ast.BitAnd, (7, 8))
    , (Ast.Eq, (9, 10))
    , (Ast.Ne, (9, 10))
    , (Ast.Lt, (11, 12))
    , (Ast.Le, (11, 12))
    , (Ast.Gt, (11, 12))
    , (Ast.Ge, (11, 12))
    , (Ast.LeftShift, (13, 14))
    , (Ast.RightShift, (15, 16))
    , (Ast.Plus, (17, 18))
    , (MinusB, (17, 18))
    , (Ast.Mul, (19, 20))
    , (Ast.Div, (19, 20))
    , (Ast.Mod, (19, 20))
    ]
