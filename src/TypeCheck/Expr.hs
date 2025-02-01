{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TypeCheck.Expr (exprTC) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Typed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Expr (Expr (..))
import Control.Monad (forM)
import Data.Text (Text)
import TypeCheck.Operators (unifyBOp, unifyUOp)
import TypeCheck.Types (AstTyped (..), CiType (..), HasType (getType), TypeM)
import TypeCheck.Utils (apply, deref, getField, nameTC, typeTC, unify)

exprTC :: Ast Named (Expr Named) -> TypeM (Ast Typed (Expr Typed))
exprTC (AstSrc sp e) = (uncurry (AstTyped sp) <$>) $ case e of
    EVar name -> do
        ty <- nameTC $ AstSrc sp name
        return (getType ty, EVar name)
    EInt i -> return (Int (i == 0), EInt i)
    EChar c -> return (Char, EChar c)
    EString t -> return (Ptr Char, EString t)
    EBinOp (AstSrc sp' op) lhs rhs -> do
        lhs' <- exprTC lhs
        rhs' <- exprTC rhs
        ty <- unifyBOp op sp (getType lhs') (getType rhs')
        return (ty, EBinOp (AstTyped sp' Void op) lhs' rhs')
    EUnOp (AstSrc sp' op) expr -> do
        expr' <- exprTC expr
        ty <- unifyUOp op sp' $ getType expr'
        return (ty, EUnOp (AstTyped sp' Void op) expr')
    ECall name exprs -> do
        f <- nameTC name
        exprs' <- forM exprs exprTC
        case underlay @_ @Text name of
            t
                | t == "printf" -> return (Int False, ECall f exprs')
                | otherwise -> do
                    ty <- apply sp f exprs'
                    return (ty, ECall f exprs')
    ENew ty expr -> do
        expr' <- exprTC expr
        ty' <- typeTC ty
        unify expr' expr' (Int False)
        return (Ptr $ getType ty', ENew ty' expr')
    EArrayAccess name idx field -> do
        name' <- nameTC name
        idx' <- exprTC idx
        unify idx' idx' (Int False)
        struct <- deref name'
        ty <- getField sp struct $ underlay field
        return
            ( ty
            , EArrayAccess name' idx' $
                (\(AstSrc sp' f) -> AstTyped sp' Void f) <$> field
            )
