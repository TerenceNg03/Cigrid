{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse.Stmt (stmtP) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Parsed))
import Ast.AstSrc (AstSrc (AstSrc))
import qualified Ast.BOp as BOp
import Ast.Expr (Expr (EArrayAccess, EBinOp, EInt, EVar))
import Ast.Stmt (Stmt (SArrayAssign, SBreak, SDelete, SExpr, SIf, SReturn, SScope, SVarAssign, SVarDef, SWhile))
import Ast.Token (Token (..))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import Fmt ((+|), (|+))
import Parse.CTypes (typeP)
import Parse.Expr (exprP)
import Parse.Types (ParseM)
import Parse.Utils (eof, expectIdent, expectP, peek, skip, throw)
import Source (HasPosition (pos), (<->))

arrayAssignLVP :: ParseM (Ast Parsed Text, Ast Parsed (Expr Parsed), Maybe (Ast Parsed Text))
arrayAssignLVP = do
    ts <- peek
    case underlay <$> ts of
        (Identifier name : LBracket : _) -> do
            skip 2
            idx <- exprP 0
            void $ expectP RBracket
            ts' <- peek
            case underlay <$> ts' of
                (Period : Identifier field : _) -> do
                    skip 2
                    return (head ts $> name, idx, Just (ts' !! 1 $> field))
                _ -> return (head ts $> name, idx, Nothing)
        _ -> error "unreachable"

varAssignP :: Bool -> ParseM (Ast Parsed (Stmt Parsed))
varAssignP canDeclare = do
    ts <- peek
    case underlay <$> ts of
        (Identifier _ : LParenthesis : _) -> do
            expr <- exprP 0
            return $ AstSrc (head ts <-> expr) $ SExpr expr
        (Identifier name : Assign : _) -> do
            skip 2
            expr <- exprP 0
            return $ AstSrc (head ts <-> expr) $ SVarAssign (head ts $> name) expr
        (Identifier name : Increment : _) -> do
            skip 2
            let sp = head ts <-> (ts !! 1)
                expr :: Ast Parsed (Expr Parsed)
                expr =
                    AstSrc
                        sp
                        (EBinOp (AstSrc sp BOp.Plus) (AstSrc sp $ EVar name) (AstSrc sp $ EInt 1))
            return $ AstSrc (head ts <-> expr) $ SVarAssign (head ts $> name) expr
        (Identifier name : Decrement : _) -> do
            skip 2
            let sp = head ts <-> (ts !! 1)
                expr :: Ast Parsed (Expr Parsed)
                expr =
                    AstSrc
                        sp
                        (EBinOp (AstSrc sp BOp.MinusB) (AstSrc sp $ EVar name) (AstSrc sp $ EInt 1))
            return $ AstSrc (head ts <-> expr) $ SVarAssign (head ts $> name) expr
        (Identifier _ : LBracket : _) -> do
            (name, idx, field) <- arrayAssignLVP
            ts' <- peek
            case underlay <$> ts' of
                (Assign : _) -> do
                    skip 1
                    value <- exprP 0
                    return $
                        AstSrc (head ts <-> value) $
                            SArrayAssign name idx field value
                (Increment : _) -> do
                    skip 1
                    let sp = head ts <-> head ts'
                        lexpr :: Ast Parsed (Expr Parsed)
                        lexpr = AstSrc sp $ EArrayAccess name idx field
                        rvalue :: Ast Parsed (Expr Parsed)
                        rvalue =
                            AstSrc
                                sp
                                (EBinOp (AstSrc sp BOp.Plus) lexpr (AstSrc sp $ EInt 1))
                    return $
                        AstSrc (head ts <-> head ts') $
                            SArrayAssign name idx field rvalue
                (Decrement : _) -> do
                    skip 1
                    let sp = head ts <-> head ts'
                        lexpr :: Ast Parsed (Expr Parsed)
                        lexpr = AstSrc sp $ EArrayAccess name idx field
                        rvalue :: Ast Parsed (Expr Parsed)
                        rvalue =
                            AstSrc
                                sp
                                (EBinOp (AstSrc sp BOp.MinusB) lexpr (AstSrc sp $ EInt 1))
                    return $
                        AstSrc (head ts <-> head ts') $
                            SArrayAssign name idx field rvalue
                (x : _) -> throw (head ts') "Expected = or ++ or -- here" $ "Got token" +| show x |+ ""
                [] -> do
                    sp <- eof
                    throw sp "Unexpected end of file" "Expected = or ++ or --"
        _ | canDeclare -> do
            ty <- typeP
            name <- expectIdent
            void $ expectP Assign
            expr <- exprP 0
            return $ AstSrc (head ts <-> expr) $ SVarDef ty name expr
        (x : _) -> throw (pos $ head ts) ("Unexpected token " +| show x |+ "") "Expected assignment statement"
        [] -> do
            sp <- eof
            throw sp "Unexpected end of file" "Expected assignment statement"

stmtP :: ParseM (Ast Parsed (Stmt Parsed))
stmtP = do
    ts <- peek
    case underlay <$> ts of
        (LBrace : _) -> do
            let consP = do
                    ts' <- peek
                    case underlay <$> ts' of
                        (RBrace : _) -> do
                            return []
                        _ -> do
                            x <- stmtP
                            xs <- consP
                            return $ x : xs
            skip 1
            body <- consP
            end <- expectP RBrace
            return $ AstSrc (head ts <-> end) $ SScope body
        (If : _) -> do
            skip 1
            void $ expectP LParenthesis
            condition <- exprP 0
            void $ expectP RParenthesis
            body <- stmtP
            ts' <- peek
            case underlay ts' of
                (Else : _) -> do
                    skip 1
                    elseBody <- stmtP
                    return $ AstSrc (head ts <-> elseBody) $ SIf condition body (Just elseBody)
                _ -> return $ AstSrc (head ts <-> body) $ SIf condition body Nothing
        (Break : _) -> do
            skip 1
            end <- expectP Semicolon
            return $ AstSrc (head ts <-> end) SBreak
        (Return : Semicolon : _) -> do
            skip 2
            return $ AstSrc (head ts <-> (ts !! 1)) $ SReturn Nothing
        (Return : _) -> do
            skip 1
            expr <- exprP 0
            end <- expectP Semicolon
            return $ AstSrc (head ts <-> end) $ SReturn $ Just expr
        (While : _) -> do
            skip 1
            void $ expectP LParenthesis
            condition <- exprP 0
            void $ expectP RParenthesis
            body <- stmtP
            return $ AstSrc (head ts <-> body) $ SWhile condition body
        (Delete : _) -> do
            skip 1
            void $ expectP LBracket
            void $ expectP RBracket
            name <- expectIdent
            end <- expectP Semicolon
            return $ AstSrc (head ts <-> end) $ SDelete name
        (For : _) -> do
            skip 1
            void $ expectP LParenthesis
            initS <- varAssignP True
            void $ expectP Semicolon
            condition <- exprP 0
            void $ expectP Semicolon
            postS <- varAssignP False
            void $ expectP RParenthesis
            body <- stmtP
            let sp = head ts <-> body
            return $
                AstSrc sp $
                    SScope
                        [ initS
                        , AstSrc sp $
                            SWhile condition (AstSrc sp $ SScope [body, postS])
                        ]
        _ -> do
            (AstSrc sp stmt) <- varAssignP True
            end <- expectP Semicolon
            return $ AstSrc (sp <-> end) stmt
