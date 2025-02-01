{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ast.UOp (UOp (..)) where

import Prettyprinter (Doc, Pretty, pretty)

data UOp = Not | BitNot | MinusU
    deriving (Eq, Ord)

instance Pretty UOp where
    pretty :: UOp -> Doc ann
    pretty Not = "!"
    pretty BitNot = "~"
    pretty MinusU = "-"
