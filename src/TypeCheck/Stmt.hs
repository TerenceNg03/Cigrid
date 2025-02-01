{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module TypeCheck.Stmt (stmtTC) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Typed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Stmt (Stmt (..))
import Control.Monad (forM, void)
import Control.Monad.Except (throwError)
import Control.Monad.State (MonadState (get), gets, put)
import Error.Diagnose (Marker (This), Report (Err))
import TypeCheck.Expr (exprTC)
import TypeCheck.Types (AstTyped (..), CiType (..), TypeM, TypeState (canBreak, returnType))
import TypeCheck.Utils (deref, getField, nameTC, registerName, typeTC, unify)

stmtTC :: Ast Named (Stmt Named) -> TypeM (Ast Typed (Stmt Typed))
stmtTC (AstSrc sp s) = (AstTyped sp Void <$>) $ case s of
    SExpr expr -> do
        expr' <- exprTC expr
        return $ SExpr expr'
    SVarDef ty name expr -> do
        ty' <- typeTC ty
        name' <- registerName name ty'
        expr' <- exprTC expr
        unify sp ty' expr'
        return $ SVarDef ty' name' expr'
    SVarAssign name expr -> do
        name' <- nameTC name
        expr' <- exprTC expr
        unify sp name' expr'
        return $ SVarAssign name' expr'
    SArrayAssign name idx field value -> do
        name' <- nameTC name
        idx' <- exprTC idx
        unify idx' idx' (Int False)
        struct <- deref name'
        ty <- getField sp struct $ underlay field
        value' <- exprTC value
        unify value' ty value'
        return $
            SArrayAssign
                name'
                idx'
                ((\(AstSrc sp' f) -> AstTyped sp' Void f) <$> field)
                value'
    SScope stmts -> do
        stmts' <- forM stmts stmtTC
        return $ SScope stmts'
    SIf cond body elseBody -> do
        cond' <- exprTC cond
        unify cond' cond' (Int False)
        body' <- stmtTC body
        case elseBody of
            Just elseBody' -> do
                elseBody'' <- stmtTC elseBody'
                return $ SIf cond' body' (Just elseBody'')
            Nothing -> return $ SIf cond' body' Nothing
    SWhile cond body -> do
        cond' <- exprTC cond
        unify cond' cond' (Int False)
        st <- get
        put st{canBreak = True}
        body' <- stmtTC body
        st' <- get
        put st'{canBreak = False}
        return $ SWhile cond' body'
    SReturn expr -> do
        ty <- gets returnType
        case expr of
            Just expr' -> do
                expr'' <- exprTC expr'
                unify sp expr'' ty
                return $ SReturn $ Just expr''
            Nothing -> do
                unify sp Void ty
                return $ SReturn Nothing
    SDelete name -> do
        name' <- nameTC name
        void $ deref name'
        return $ SDelete name'
    SBreak -> do
        b <- gets canBreak
        if b
            then return SBreak
            else do
                throwError $
                    (sp,) $
                        Err
                            Nothing
                            "Break without a loop"
                            [(sp, This "While type checking this")]
                            []
