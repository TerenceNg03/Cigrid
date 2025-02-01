{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast.Expr (Expr (..)) where

import Ast.Ast (Ast, IsAst, Phase, underlay)
import Ast.BOp (BOp)
import Ast.CType (CType)
import Ast.ShowText (showChar, showText)
import Ast.UOp (UOp)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty, encloseSep, pretty)

data Expr (p :: Phase)
    = EVar Text
    | EInt Int
    | EChar Char
    | EString Text
    | EBinOp (Ast p BOp) (Ast p (Expr p)) (Ast p (Expr p))
    | EUnOp (Ast p UOp) (Ast p (Expr p))
    | ECall (Ast p Text) [Ast p (Expr p)]
    | ENew (Ast p (CType p)) (Ast p (Expr p))
    | EArrayAccess (Ast p Text) (Ast p (Expr p)) (Maybe (Ast p Text))

deriving instance
    ( Eq (Ast p (Expr p))
    , Eq (Ast p BOp)
    , Eq (Ast p UOp)
    , Eq (Ast p Text)
    , Eq (Ast p (CType p))
    ) =>
    Eq (Expr p)

instance
    ( IsAst (Ast a (Expr a)) (Expr a)
    , IsAst (Ast a BOp) BOp
    , IsAst (Ast a UOp) UOp
    , IsAst (Ast a Text) Text
    , IsAst (Ast a (CType a)) (CType a)
    ) =>
    Pretty (Expr a)
    where
    pretty :: Expr a -> Doc ann
    pretty (EVar s) = "EVar" <> "(" <> showText s <> ")"
    pretty (EInt i) = "EInt" <> "(" <> pretty i <> ")"
    pretty (EChar c) = "EChar" <> "(" <> Ast.ShowText.showChar c <> ")"
    pretty (EString s) = "EString" <> "(" <> showText s <> ")"
    pretty (EBinOp op left right) =
        "EBinOp"
            <> "("
            <> pretty (underlay @_ @BOp op)
            <> ", "
            <> pretty (underlay @_ @(Expr a) left)
            <> ", "
            <> pretty (underlay @_ @(Expr a) right)
            <> ")"
    pretty (EUnOp op expr) =
        "EUnOp"
            <> "("
            <> pretty (underlay @_ @UOp op)
            <> ", "
            <> pretty (underlay @_ @(Expr a) expr)
            <> ")"
    pretty (ECall fun exprs) =
        "ECall"
            <> "("
            <> showText (underlay @_ @Text fun)
            <> ","
            <> encloseSep "{" "}" " " (pretty . underlay @_ @(Expr a) <$> exprs)
            <> ")"
    pretty (ENew ty expr) =
        "ENew"
            <> "("
            <> pretty (underlay @_ @(CType a) ty)
            <> ", "
            <> pretty (underlay @_ @(Expr a) expr)
            <> ")"
    pretty (EArrayAccess arr idx field) =
        "EArrayAccess"
            <> "("
            <> showText (underlay @_ @Text arr)
            <> ", "
            <> pretty (underlay @_ @(Expr a) idx)
            <> ", "
            <> case field of
                Just field' -> showText (underlay @_ @Text field')
                Nothing -> ""
            <> ")"

instance
    ( IsAst (Ast a (Expr a)) (Expr a)
    , IsAst (Ast a BOp) BOp
    , IsAst (Ast a UOp) UOp
    , IsAst (Ast a Text) Text
    , IsAst (Ast a (CType a)) (CType a)
    ) =>
    Show (Expr a)
    where
    show :: Expr a -> String
    show = show . pretty
