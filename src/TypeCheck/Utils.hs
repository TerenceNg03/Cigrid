{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TypeCheck.Utils (unify, apply, nameTC, deref, getField, registerName, typeTC, typeTC') where

import Ast.Ast (Ast, Phase (Named, Typed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.CType (CType (..))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), gets, put)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Error.Diagnose (Marker (This), Position, Report (Err))
import Fmt ((+|), (|+))
import Source (HasPosition (pos))
import TypeCheck.Types (AstTyped (AstTyped), CiType (..), HasType (getType), TypeM, TypeState (..))

apply :: (HasType x, HasType y) => Position -> x -> [y] -> TypeM CiType
apply sp x (y : ys) = case getType x of
    (Arrow t1 t2) -> do
        unify sp t1 (getType y)
        apply sp t2 ys
    xty -> do
        throwError $
            (sp,) $
                Err
                    Nothing
                    ("Can not apply type \"" +| show (getType y) |+ "\" to type \"" +| show xty |+ "\"")
                    [(sp, This "While type checking this")]
                    []
apply sp x [] = case getType x of
    (Arrow _ _) -> do
        throwError $
            (sp,) $
                Err
                    Nothing
                    "Currying is not allowed"
                    [(sp, This "While type checking this")]
                    []
    xty -> return xty

unify :: (HasPosition s, HasType x, HasType y) => s -> x -> y -> TypeM ()
unify sp' x y = do
    let xty = getType x
        yty = getType y
        sp = pos sp'
    if xty == yty
        then return ()
        else do
            throwError $
                (sp,) $
                    Err
                        Nothing
                        ("Can not unify type \"" +| show xty |+ "\" with type \"" +| show yty |+ "\"")
                        [(sp, This "While type checking this")]
                        []
typeTC :: Ast Named (CType Named) -> TypeM (Ast Typed (CType Typed))
typeTC = typeTC' False

typeTC' :: Bool -> Ast Named (CType Named) -> TypeM (Ast Typed (CType Typed))
typeTC' isZero (AstSrc sp t) = do
    st <- gets structs
    case t of
        TInt -> return $ AstTyped sp (Int isZero) TInt
        TChar -> return $ AstTyped sp Char TChar
        TVoid -> return $ AstTyped sp Void TVoid
        TPoint t' -> do
            t'' <- typeTC t'
            return $ AstTyped sp (Ptr $ getType t'') $ TPoint t''
        TIdent t' -> do
            if Set.member t' st
                then return $ AstTyped sp (Struct t') (TIdent t')
                else do
                    throwError $
                        (sp,) $
                            Err
                                Nothing
                                ("Invalid type \"" +| show (Struct t') |+ "\"")
                                [(sp, This "While type checking this")]
                                []

nameTC :: Ast Named Text -> TypeM (Ast Typed Text)
nameTC (AstSrc sp name) = do
    m <- gets names
    let ty = m Map.! name
    return $ AstTyped sp ty name

deref :: Ast Typed Text -> TypeM CiType
deref t = case getType t of
    Ptr ty -> return ty
    ty -> do
        throwError $
            (pos t,) $
                Err
                    Nothing
                    ("Can not dereference type \"" +| show ty |+ "\"")
                    [(pos t, This "While type checking this")]
                    []

getField :: Position -> CiType -> Maybe Text -> TypeM CiType
getField _ ty Nothing = return ty
getField sp ty (Just field) = do
    m <- gets structFields
    case ty of
        (Struct sty) | isJust $ Map.lookup (sty, field) m -> return $ m Map.! (sty, field)
        _ -> do
            throwError $
                (sp,) $
                    Err
                        Nothing
                        ("Can not access field \"" +| field |+ "\" of type \"" +| show ty |+ "\"")
                        [(sp, This "While type checking this")]
                        []

registerName :: (HasType b) => Ast Named Text -> b -> TypeM (Ast Typed Text)
registerName (AstSrc sp name) ty' = do
    let ty = getType ty'
    state <- get
    m <- gets names
    case Map.lookup name m of
        Just tym -> do
            unify sp ty tym
            return $ AstTyped sp ty name
        Nothing -> do
            put $ state{names = Map.insert name ty $ names state}
            return $ AstTyped sp ty name
