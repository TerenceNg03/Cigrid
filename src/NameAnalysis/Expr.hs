{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NameAnalysis.Expr (resolveExpr) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Parsed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Expr (Expr (..))
import Control.Monad (forM)
import Data.Functor (($>))
import NameAnalysis.Types (NameM)
import NameAnalysis.Utils (resolve, resolveType)
import Parse.Types ()

resolveExpr :: Ast Parsed (Expr Parsed) -> NameM (Ast Named (Expr Named))
resolveExpr e = ((e $>) <$>) $ case underlay @_ @(Expr Parsed) e of
    EVar name -> do
        name' <- resolve (e $> name)
        return $ EVar (underlay name')
    EInt i -> return $ EInt i
    EChar c -> return $ EChar c
    EString t -> return $ EString t
    EBinOp op lhs rhs -> do
        lhs' <- resolveExpr lhs
        rhs' <- resolveExpr rhs
        return $ EBinOp op lhs' rhs'
    EUnOp op expr -> do
        expr' <- resolveExpr expr
        return $ EUnOp op expr'
    ECall name exprs -> do
        name' <- resolve name
        exprs' <- forM exprs resolveExpr
        return $ ECall name' exprs'
    ENew ty expr -> do
        expr' <- resolveExpr expr
        return $ ENew (resolveType ty) expr'
    EArrayAccess name expr field -> do
        name' <- resolve name
        expr' <- resolveExpr expr
        let field' = (\(AstSrc sp f) -> AstSrc sp f) <$> field
        return $ EArrayAccess name' expr' field'
