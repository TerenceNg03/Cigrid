{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ast.BOp (BOp (..)) where

import Prettyprinter (Doc, Pretty, pretty)

data BOp
    = Plus
    | MinusB
    | Mul
    | Div
    | Mod
    | Lt
    | Le
    | Ge
    | Gt
    | Eq
    | Ne
    | And
    | Or
    | BitAnd
    | BitOr
    | LeftShift
    | RightShift
    deriving (Eq, Ord)

instance Pretty BOp where
    pretty :: BOp -> Doc ann
    pretty Plus = "+"
    pretty MinusB = "-"
    pretty Mul = "*"
    pretty Div = "/"
    pretty Mod = "%"
    pretty Lt = "<"
    pretty Le = "<="
    pretty Ge = ">="
    pretty Gt = ">"
    pretty Eq = "=="
    pretty Ne = "!="
    pretty And = "&&"
    pretty Or = "||"
    pretty BitAnd = "&"
    pretty BitOr = "|"
    pretty LeftShift = "<<"
    pretty RightShift = ">>"

instance Show BOp where
    show :: BOp -> String
    show = show . pretty
