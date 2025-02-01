{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast.Global (Global (..)) where

import Ast.Ast (Ast, IsAst (underlay), Phase)
import Ast.BOp (BOp)
import Ast.CType (CType)
import Ast.Expr (Expr)
import Ast.ShowText (showText)
import Ast.Stmt (Stmt)
import Ast.UOp (UOp)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), encloseSep, indent, line)

data Global (p :: Phase)
    = GFuncDef (Ast p (CType p)) (Ast p Text) [(Ast p (CType p), Ast p Text)] (Ast p (Stmt p))
    | GFuncDecl (Ast p (CType p)) (Ast p Text) [(Ast p (CType p), Ast p Text)]
    | GVarDef (Ast p (CType p)) (Ast p Text) (Ast p (Expr p))
    | GVarDecl (Ast p (CType p)) (Ast p Text)
    | GStruct (Ast p Text) [(Ast p (CType p), Ast p Text)]

deriving instance
    ( Eq (Ast a (CType a))
    , Eq (Ast a Text)
    , Eq (Ast a (Stmt a))
    , Eq (Ast a (Expr a))
    , Eq (Ast a BOp)
    , Eq (Ast a UOp)
    ) =>
    Eq (Global a)

instance
    ( IsAst (Ast a (CType a)) (CType a)
    , IsAst (Ast a Text) Text
    , IsAst (Ast a (Stmt a)) (Stmt a)
    , IsAst (Ast a (Expr a)) (Expr a)
    , IsAst (Ast a BOp) BOp
    , IsAst (Ast a UOp) UOp
    ) =>
    Pretty (Global a)
    where
    pretty :: Global a -> Doc ann
    pretty (GFuncDef retType name params body) =
        "GFuncDef"
            <> "("
            <> pretty (underlay @_ @(CType a) retType)
            <> ", "
            <> showText (underlay @_ @Text name)
            <> ", "
            <> encloseSep "{" "}" " " (prettyParam <$> params)
            <> ","
            <> line
            <> indent 2 (pretty (underlay @_ @(Stmt a) body))
            <> ")"
            <> line
            <> line
      where
        prettyParam (ty, paramName) =
            "(" <> pretty (underlay @_ @(CType a) ty) <> "," <> pretty (show $ underlay @_ @Text paramName) <> ")"
    pretty (GFuncDecl retType name params) =
        "GFuncDecl"
            <> "("
            <> pretty (underlay @_ @(CType a) retType)
            <> ", "
            <> showText (underlay @_ @Text name)
            <> ", "
            <> encloseSep "{" "}" " " (prettyParam <$> params)
            <> ")"
            <> line
            <> line
      where
        prettyParam (ty, paramName) =
            "(" <> pretty (underlay @_ @(CType a) ty) <> "," <> showText (underlay @_ @Text paramName) <> ")"
    pretty (GVarDef varType name expr) =
        "GVarDef"
            <> "("
            <> pretty (underlay @_ @(CType a) varType)
            <> ", "
            <> showText (underlay @_ @Text name)
            <> ", "
            <> pretty (underlay @_ @(Expr a) expr)
            <> ")"
            <> line
            <> line
    pretty (GVarDecl varType name) =
        "GVarDecl"
            <> "("
            <> pretty (underlay @_ @(CType a) varType)
            <> ", "
            <> showText (underlay @_ @Text name)
            <> ")"
            <> line
            <> line
    pretty (GStruct structName fields) =
        "GStruct"
            <> "("
            <> showText (underlay @_ @Text structName)
            <> ",{"
            <> line
            <> indent 2 (encloseSep "" "" "" (prettyField <$> fields))
            <> "})"
            <> line
            <> line
      where
        prettyField (fieldType, fieldName) =
            "(" <> pretty (underlay @_ @(CType a) fieldType) <> ", " <> showText (underlay @_ @Text fieldName) <> ")"

instance
    ( IsAst (Ast a (CType a)) (CType a)
    , IsAst (Ast a Text) Text
    , IsAst (Ast a (Stmt a)) (Stmt a)
    , IsAst (Ast a (Expr a)) (Expr a)
    , IsAst (Ast a BOp) BOp
    , IsAst (Ast a UOp) UOp
    ) =>
    Show (Global a)
    where
    show :: Global a -> String
    show = show . pretty
