{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NameAnalysis.Utils (resolve, define, declare, withAnonyScope, withScope, resolveType) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Parsed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.CType (CType (..))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), get, gets, put)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Text (Text, intercalate)
import Error.Diagnose (Marker (This, Where), Position, Report (Err))
import Fmt ((+|), (|+))
import NameAnalysis.Types (NameM, NameState (..), Scope (..))
import Parse.Types ()

toId :: [Text] -> Text -> Text
toId names name = intercalate "." $ reverse (name : names)

putSName :: [Text] -> NameM ()
putSName scope = do
    s <- get
    put $ s{sname = scope}

putGenId :: Int -> NameM ()
putGenId i = do
    s <- get
    put $ s{genId = i}

updateResolved :: Text -> (NameState, Position) -> NameM ()
updateResolved k v = do
    s <- get
    put $ s{resolved = Map.insert k v (resolved s)}

withScope :: Text -> NameM a -> NameM a
withScope t m = do
    name <- gets sname
    putSName $ t : name
    x <- m
    putSName name
    return x

withAnonyScope :: NameM a -> NameM a
withAnonyScope m = do
    nextId <- gets genId
    name <- gets sname
    let t = "$" +| nextId |+ ""
    putGenId (nextId + 1)
    putSName $ t : name
    x <- m
    putSName name
    return x

register :: NameState -> Ast Parsed Text -> NameM (Ast Parsed Text)
register ns (AstSrc sp t) = do
    scope <- gets sname
    names <- gets resolved
    let newId = toId scope t
    case (ns, Map.lookup newId names) of
        (_, Just (Defined, sp')) -> do
            throwError $
                (sp,) $
                    Err
                        Nothing
                        ("Name " +| show t |+ " is already defined.")
                        [ (sp', Where "Previously defined here")
                        , (sp, This "Defined again here")
                        ]
                        []
        _ -> do
            updateResolved newId (ns, sp)
            return $ AstSrc sp newId

declare :: Ast Parsed Text -> NameM (Ast Parsed Text)
declare = register Declared

define :: Ast Parsed Text -> NameM (Ast Parsed Text)
define = register Defined

resolveMaybe :: [Text] -> Text -> NameM (Maybe [Text])
resolveMaybe scope@(_ : scopeXs) name = do
    let absId = toId scope name
    names <- gets resolved
    case Map.lookup absId names of
        Just _ -> return $ Just scope
        Nothing -> resolveMaybe scopeXs name
resolveMaybe [] name = do
    let absId = toId [] name
    names <- gets resolved
    case Map.lookup absId names of
        Just _ -> return $ Just []
        Nothing -> return Nothing

resolve :: Ast Parsed Text -> NameM (Ast Parsed Text)
resolve (AstSrc sp t) = do
    scope <- gets sname
    result <- resolveMaybe scope t
    case result of
        Just scope' -> return $ AstSrc sp (toId scope' t)
        Nothing -> do
            throwError $
                (sp,) $
                    Err
                        Nothing
                        ("Name " +| show t |+ " is not defined.")
                        [(sp, This "Mentioned here")]
                        []

resolveType :: Ast Parsed (CType Parsed) -> Ast Named (CType Named)
resolveType ty =
    case underlay @_ @(CType Parsed) ty of
        TVoid -> ty $> TVoid
        TInt -> ty $> TInt
        TChar -> ty $> TChar
        TIdent t -> ty $> TIdent t
        TPoint ty' -> ty $> TPoint (resolveType ty')
