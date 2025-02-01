{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Expr (exprP) where

import Ast.Ast (Ast, Phase (Parsed), underlay)
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Expr (Expr (..))
import Ast.Token (Token (..))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Map ((!))
import Data.Maybe (fromJust, isJust)
import Fmt ((+|), (|+))
import Operator (infixPre, maybeInfix, maybePrefix, prefixPre)
import Parse.CTypes (typeP)
import Parse.Types (ParseM)
import Parse.Utils (eof, expectP, listP, peek, skip, throw)
import Source (HasPosition (pos), (<->))
import Prelude hiding (lex)

type instance Ast Parsed a = AstSrc a

binOpP :: Ast Parsed (Expr Parsed) -> Int -> ParseM (Ast Parsed (Expr Parsed))
binOpP lhs minPre = do
    ts <- peek
    case underlay <$> ts of
        (t : _)
            | isJust $ maybeInfix t -> do
                let op = fromJust $ maybeInfix t
                    (lpre, rpre) = infixPre ! op
                if lpre < minPre
                    then return lhs
                    else do
                        skip 1
                        rhs <- exprP rpre
                        let lhs' = AstSrc (lhs <-> rhs) $ EBinOp (head ts $> op) lhs rhs
                        binOpP lhs' minPre
        _ -> return lhs

exprP :: Int -> ParseM (Ast Parsed (Expr Parsed))
exprP minPre = do
    ts <- peek
    lhs <- case underlay <$> ts of
        (LParenthesis : _) -> do
            let start = head ts
            skip 1
            inner <- exprP 0
            end <- expectP RParenthesis
            return $ AstSrc (start <-> end) $ underlay inner
        (Identifier name : LParenthesis : _) -> do
            skip 1
            content <- listP LParenthesis RParenthesis $ exprP 0
            return $ AstSrc (head ts <-> content) $ ECall (head ts $> name) (underlay content)
        (Identifier name : LBracket : _) -> do
            skip 2
            idx <- exprP 0
            end <- expectP RBracket
            ts' <- peek
            field <- case underlay <$> ts' of
                (Period : Identifier field : _) -> skip 2 >> return (Just $ ts' !! 1 $> field)
                _ -> return Nothing
            return $
                AstSrc (head ts <-> maybe end pos field) $
                    EArrayAccess (head ts $> name) idx field
        (New : _) -> do
            skip 1
            ty <- typeP
            void $ expectP LBracket
            expr <- exprP 0
            end <- expectP RBracket
            return $ AstSrc (head ts <-> end) $ ENew ty expr
        (t : _)
            | isJust $ maybePrefix t -> do
                let op = fromJust $ maybePrefix t
                    (_, pre) = prefixPre ! op
                    opNode = head ts
                skip 1
                expr <- exprP pre
                return $ AstSrc (opNode <-> expr) $ EUnOp (opNode $> op) expr
        (Identifier name : _) -> skip 1 >> return (head ts $> EVar name)
        (Integer i : _) -> skip 1 >> return (head ts $> EInt i)
        (Char c : _) -> skip 1 >> return (head ts $> EChar c)
        (String s : _) -> skip 1 >> return (head ts $> EString s)
        (x : _) -> throw (pos $ head ts) ("Unexpected token " +| show x |+ "") "Expected expression"
        [] -> do
            sp <- eof
            throw sp "Unexpected end of file" "Expected expression"
    binOpP lhs minPre
