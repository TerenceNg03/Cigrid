{-# LANGUAGE DataKinds #-}

module Parse.Global (globalP, globalsP) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Parsed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.CType (CType)
import Ast.Global (Global (GFuncDecl, GFuncDef, GStruct, GVarDecl, GVarDef))
import Ast.Token (Token (..))
import Control.Monad (void)
import Data.Text (Text)
import Parse.CTypes (typeP)
import Parse.Expr (exprP)
import Parse.Stmt (stmtP)
import Parse.Types (ParseM)
import Parse.Utils (expectIdent, expectP, listP, peek, skip)
import Source ((<->))

paramP :: ParseM (Ast Parsed (Ast Parsed (CType Parsed), Ast Parsed Text))
paramP = do
    ty <- typeP
    name <- expectIdent
    return $ AstSrc (ty <-> name) (ty, name)

paramStructP :: ParseM [(Ast Parsed (CType Parsed), Ast Parsed Text)]
paramStructP = do
    ts <- peek
    case underlay <$> ts of
        (RBrace : _) -> do
            return []
        _ -> do
            x <- paramP
            void $ expectP Semicolon
            xs <- paramStructP
            return (underlay x : xs)

globalP :: ParseM (Ast Parsed (Global Parsed))
globalP = do
    ts <- peek
    case underlay <$> ts of
        (Extern : _) -> do
            skip 1
            ty <- typeP
            name <- expectIdent
            ts' <- peek
            case underlay <$> ts' of
                (LParenthesis : _) -> do
                    AstSrc _ params <- listP LParenthesis RParenthesis paramP
                    end <- expectP Semicolon
                    return $ AstSrc (head ts <-> end) $ GFuncDecl ty name $ underlay <$> params
                _ -> do
                    end <- expectP Semicolon
                    return $ AstSrc (head ts <-> end) $ GVarDecl ty name
        (Struct : _) -> do
            skip 1
            name <- expectIdent
            void $ expectP LBrace
            fields <- paramStructP
            void $ expectP RBrace
            end <- expectP Semicolon
            return $ AstSrc (head ts <-> end) $ GStruct name fields
        _ -> do
            ty <- typeP
            name <- expectIdent
            ts' <- peek
            case underlay <$> ts' of
                (LParenthesis : _) -> do
                    AstSrc _ params <- listP LParenthesis RParenthesis paramP
                    stmt <- stmtP
                    return $ AstSrc (head ts <-> stmt) $ GFuncDef ty name (underlay <$> params) stmt
                _ -> do
                    void $ expectP Assign
                    expr <- exprP 0
                    end <- expectP Semicolon
                    return $ AstSrc (head ts <-> end) $ GVarDef ty name expr

globalsP :: ParseM [Ast Parsed (Global Parsed)]
globalsP = do
    ts <- peek
    case ts of
        (_ : _) -> do
            x <- globalP
            xs <- globalsP
            return (x : xs)
        [] -> return []
