{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NameAnalysis.Global (resolveGlbs) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Parsed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Global (Global (..))
import Control.Monad (forM)
import Data.Functor (($>))
import NameAnalysis.Expr (resolveExpr)
import NameAnalysis.Stmt (resolveStmtNoScope)
import NameAnalysis.Types (NameM)
import NameAnalysis.Utils (declare, define, resolveType, withAnonyScope, withScope)
import Parse.Types ()

resolveGlb :: Ast Parsed (Global Parsed) -> NameM (Ast Named (Global Named))
resolveGlb g = ((g $>) <$>) $ case underlay @_ @(Global Parsed) g of
    GFuncDef ty name params body -> do
        let ty' = resolveType ty
        name' <- define name
        (params', body') <- withScope (underlay name) $ do
            params' <- forM params $ \(pty, param) -> do
                let pty' = resolveType pty
                param' <- define param
                return (pty', param')
            body' <- resolveStmtNoScope body
            return (params', body')
        return $ GFuncDef ty' name' params' body'
    GFuncDecl ty name params -> do
        let ty' = resolveType ty
        name' <- declare name
        params' <- withAnonyScope $
            forM params $ \(pty, param) -> do
                let pty' = resolveType pty
                param' <- define param
                return (pty', param')
        return $ GFuncDecl ty' name' params'
    GVarDef ty name expr -> do
        let ty' = resolveType ty
        name' <- define name
        expr' <- resolveExpr expr
        return $ GVarDef ty' name' expr'
    GVarDecl ty name -> do
        let ty' = resolveType ty
        name' <- declare name
        return $ GVarDecl ty' name'
    GStruct name fields -> do
        name' <- define name
        let fields' = (\(ty, AstSrc sp f) -> (resolveType ty, AstSrc sp f)) <$> fields
        return $ GStruct name' fields'

resolveGlbs :: [Ast Parsed (Global Parsed)] -> NameM [Ast Named (Global Named)]
resolveGlbs = mapM resolveGlb
