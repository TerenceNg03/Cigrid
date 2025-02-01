{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NameAnalysis.Stmt (resolveStmt, resolveStmtNoScope) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Parsed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Stmt (Stmt (..))
import Control.Monad (forM)
import Data.Functor (($>))
import NameAnalysis.Expr (resolveExpr)
import NameAnalysis.Types (NameM)
import NameAnalysis.Utils (define, resolve, resolveType, withAnonyScope)
import Parse.Types ()

resolveStmt :: AstSrc (Stmt Parsed) -> NameM (AstSrc (Stmt Named))
resolveStmt = resolveStmt' False

resolveStmtNoScope :: AstSrc (Stmt Parsed) -> NameM (AstSrc (Stmt Named))
resolveStmtNoScope = resolveStmt' True

resolveStmt' :: Bool -> Ast Parsed (Stmt Parsed) -> NameM (Ast Named (Stmt Named))
resolveStmt' noScope s = ((s $>) <$>) $ case underlay @_ @(Stmt Parsed) s of
    SExpr expr -> do
        expr' <- resolveExpr expr
        return $ SExpr expr'
    SVarDef ty name expr -> do
        name' <- define name
        expr' <- resolveExpr expr
        return $ SVarDef (resolveType ty) name' expr'
    SVarAssign name expr -> do
        name' <- resolve name
        expr' <- resolveExpr expr
        return $ SVarAssign name' expr'
    SArrayAssign name idx field value -> do
        name' <- resolve name
        idx' <- resolveExpr idx
        let field' = (\(AstSrc sp f) -> AstSrc sp f) <$> field
        value' <- resolveExpr value
        return $ SArrayAssign name' idx' field' value'
    SScope stmts -> do
        stmts' <-
            if noScope
                then forM stmts resolveStmt
                else withAnonyScope $ forM stmts resolveStmt
        return $ SScope stmts'
    SIf cond body elseBody -> do
        cond' <- resolveExpr cond
        body' <- resolveStmt body
        case elseBody of
            Just elseBody' -> do
                elseBody'' <- resolveStmt elseBody'
                return $ SIf cond' body' (Just elseBody'')
            Nothing -> return $ SIf cond' body' Nothing
    SWhile cond body -> do
        cond' <- resolveExpr cond
        body' <- resolveStmt body
        return $ SWhile cond' body'
    SBreak -> return SBreak
    SReturn expr ->
        case expr of
            Just expr' -> do
                expr'' <- resolveExpr expr'
                return $ SReturn $ Just expr''
            Nothing -> return $ SReturn Nothing
    SDelete name -> do
        name' <- resolve name
        return $ SDelete name'
