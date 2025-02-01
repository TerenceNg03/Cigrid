{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast.Stmt (Stmt (..)) where

import Ast.Ast (Ast, IsAst (underlay), Phase)
import Ast.BOp (BOp)
import Ast.CType (CType)
import Ast.Expr (Expr)
import Ast.ShowText (showText)
import Ast.UOp (UOp)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), encloseSep, indent, line)

data Stmt (p :: Phase)
    = SExpr (Ast p (Expr p))
    | SVarDef (Ast p (CType p)) (Ast p Text) (Ast p (Expr p))
    | SVarAssign (Ast p Text) (Ast p (Expr p))
    | SArrayAssign (Ast p Text) (Ast p (Expr p)) (Maybe (Ast p Text)) (Ast p (Expr p))
    | SScope [Ast p (Stmt p)]
    | SIf (Ast p (Expr p)) (Ast p (Stmt p)) (Maybe (Ast p (Stmt p)))
    | SWhile (Ast p (Expr p)) (Ast p (Stmt p))
    | SBreak
    | SReturn (Maybe (Ast p (Expr p)))
    | SDelete (Ast p Text)

deriving instance
    ( Eq (Ast a (Stmt a))
    , Eq (Ast a (Expr a))
    , Eq (Ast a (CType a))
    , Eq (Ast a Text)
    , Eq (Ast a BOp)
    , Eq (Ast a UOp)
    ) =>
    Eq (Stmt a)

instance
    ( IsAst (Ast a (Stmt a)) (Stmt a)
    , IsAst (Ast a (Expr a)) (Expr a)
    , IsAst (Ast a (CType a)) (CType a)
    , IsAst (Ast a Text) Text
    , IsAst (Ast a BOp) BOp
    , IsAst (Ast a UOp) UOp
    ) =>
    Pretty (Stmt a)
    where
    pretty :: Stmt a -> Doc ann
    pretty (SExpr expr) =
        "SExpr"
            <> "("
            <> pretty (underlay @_ @(Expr a) expr)
            <> ")"
    pretty (SVarDef ty var expr) =
        "SVarDef"
            <> "("
            <> pretty (underlay @_ @(CType a) ty)
            <> ", "
            <> showText (underlay @_ @Text var)
            <> ", "
            <> pretty (underlay @_ @(Expr a) expr)
            <> ")"
    pretty (SVarAssign var expr) =
        "SVarAssign"
            <> "("
            <> showText (underlay @_ @Text var)
            <> ", "
            <> pretty (underlay @_ @(Expr a) expr)
            <> ")"
    pretty (SArrayAssign var expr field value) =
        "SArrayAssign"
            <> "("
            <> showText (underlay @_ @Text var)
            <> ", "
            <> pretty (underlay @_ @(Expr a) expr)
            <> ", "
            <> case field of
                Just field' -> showText (underlay @_ @Text field')
                Nothing -> " "
            <> ", "
            <> pretty
                (underlay @_ @(Expr a) value)
            <> ")"
    pretty (SScope stmts) =
        "SScope"
            <> "({"
            <> line
            <> indent 2 (encloseSep "" "" "" (pretty . underlay @_ @(Stmt a) <$> stmts))
            <> line
            <> "})"
    pretty (SIf cond thenStmt elseStmt) =
        "SIf"
            <> "("
            <> pretty (underlay @_ @(Expr a) cond)
            <> ","
            <> line
            <> indent 2 (pretty (underlay @_ @(Stmt a) thenStmt))
            <> ","
            <> case elseStmt of
                Just stmt -> line <> indent 2 (pretty (underlay @_ @(Stmt a) stmt))
                Nothing -> " "
            <> ")"
    pretty (SWhile cond stmt) =
        "SWhile"
            <> "("
            <> pretty (underlay @_ @(Expr a) cond)
            <> ","
            <> line
            <> indent 2 (pretty (underlay @_ @(Stmt a) stmt))
            <> ")"
    pretty SBreak =
        "SBreak"
    pretty (SReturn maybeExpr) =
        "SReturn"
            <> "("
            <> case maybeExpr of
                Just expr -> pretty (underlay @_ @(Expr a) expr)
                Nothing -> ""
            <> ")"
    pretty (SDelete var) =
        "SDelete"
            <> "("
            <> showText (underlay @_ @Text var)
            <> ")"

instance
    ( IsAst (Ast a (Stmt a)) (Stmt a)
    , IsAst (Ast a (Expr a)) (Expr a)
    , IsAst (Ast a (CType a)) (CType a)
    , IsAst (Ast a Text) Text
    , IsAst (Ast a BOp) BOp
    , IsAst (Ast a UOp) UOp
    ) =>
    Show (Stmt a)
    where
    show :: Stmt a -> String
    show = show . pretty
