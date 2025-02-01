{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck.Global (globalsTC) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Named, Typed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Global (Global (..))
import Control.Monad (forM)
import Control.Monad.State (MonadState (get), put)
import Data.Foldable (Foldable (foldr'))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Source (HasPosition (pos))
import TypeCheck.Expr (exprTC)
import TypeCheck.Stmt (stmtTC)
import TypeCheck.Types (AstTyped (..), CiType (..), HasType (getType), TypeM, TypeState (..))
import TypeCheck.Utils (registerName, typeTC, unify)

globalsTC :: [Ast Named (Global Named)] -> TypeM [Ast Typed (Global Typed)]
globalsTC = mapM globalTC

globalTC :: Ast Named (Global Named) -> TypeM (Ast Typed (Global Typed))
globalTC (AstSrc sp g) = (AstTyped sp Void <$>) $ case g of
    GFuncDef ty name params body -> do
        ty' <- typeTC ty
        params' <- forM params $ \(pty, param) -> do
            pty' <- typeTC pty
            param' <- registerName param pty'
            return (pty', param')
        let fty = foldr' (Arrow . getType) (getType ty') (fst <$> params')
        name' <- registerName name fty
        s <- get
        put $ s{returnType = getType ty'}
        body' <- stmtTC body
        s' <- get
        put $ s'{returnType = Void}
        return $ GFuncDef ty' name' params' body'
    GFuncDecl ty name params -> do
        ty' <- typeTC ty
        params' <- forM params $ \(pty, param) -> do
            pty' <- typeTC pty
            param' <- registerName param pty'
            return (pty', param')
        let fty = foldr' (Arrow . getType) (getType ty') (fst <$> params')
        name' <- registerName name fty
        return $ GFuncDecl ty' name' params'
    GVarDef ty name expr -> do
        ty' <- typeTC ty
        name' <- registerName name ty'
        expr' <- exprTC expr
        unify sp name' expr'
        return $ GVarDef ty' name' expr'
    GVarDecl ty name -> do
        ty' <- typeTC ty
        name' <- registerName name ty'
        return $ GVarDecl ty' name'
    GStruct name fields -> do
        let name' = AstTyped (pos name) Void (underlay name)
        s <- get
        put $ s{structs = Set.insert (underlay name) $ structs s}
        fields' <- forM fields $ \(ty, fname) -> do
            ty' <- typeTC ty
            let fname' = AstTyped (pos fname) Void (underlay fname)
            s' <- get
            put $
                s'
                    { structFields =
                        Map.insert
                            (underlay name, underlay fname)
                            (getType ty')
                            $ structFields s'
                    }
            return (ty', fname')
        return $ GStruct name' fields'
