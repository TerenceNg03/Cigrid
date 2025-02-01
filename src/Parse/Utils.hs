{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Utils (peek, eof, skip, throw, expectP, listP, expectIdent) where

import Ast.Ast (Ast, Phase (Lexed, Parsed), underlay)
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Token (Token (..))
import Data.Functor (($>))
import Data.Text (Text)
import Diagnose (reportErr)
import Error.Diagnose (Position (Position))
import Fmt ((+|), (|+))
import Lex ()
import Parse.Types (Context (Context, source, tokens), ParseM (ParseM))
import Source (HasPosition (pos), Source (filename, loc), next, (<->))
import Prelude hiding (lex)

-- | Location of Eof
eof :: ParseM Position
eof = ParseM $ \c ->
    let converge f x = if x == f x then x else converge f (f x)
        end = converge next c.source
     in Right (Position end.loc end.loc c.source.filename, c)

-- | Look Ahead
peek :: ParseM [Ast Lexed Token]
peek = ParseM $ \c -> Right (c.tokens, c)

-- | Skip n tokens
skip :: Int -> ParseM ()
skip n = ParseM $ \c -> Right ((), Context c.source $ drop n c.tokens)

-- | Throw an error with location and description
throw :: (HasPosition s) => s -> Text -> Text -> ParseM a
throw sp msg hint = ParseM $ \_ ->
    Left (pos sp, reportErr (pos sp) msg hint)

expectP :: Token -> ParseM Position
expectP t = do
    ts <- peek
    case underlay <$> ts of
        (x : _)
            | x == t -> skip 1 >> return (pos $ head ts)
            | otherwise ->
                throw
                    (head ts)
                    ("Expect token " +| show t |+ "")
                    $ "Got token " +| show x |+ ""
        [] -> do
            sp <- eof
            throw sp ("Expect token " +| show t |+ "") "End of file here"

listP :: Token -> Token -> ParseM (Ast Parsed a) -> ParseM (Ast Parsed [Ast Parsed a])
listP t1 t2 p = do
    start <- expectP t1
    ts <- peek
    case underlay <$> ts of
        (t : _) | t == t2 -> do
            skip 1
            return $ AstSrc (start <-> head ts) []
        _ -> do
            x <- p
            xs <- consP
            end <- expectP t2
            return $ AstSrc (start <-> end) (x : xs)
  where
    consP = do
        ts <- peek
        case underlay <$> ts of
            (Comma : _) -> do
                skip 1
                x <- p
                xs <- consP
                return $ x : xs
            _ -> return []

expectIdent :: ParseM (Ast Parsed Text)
expectIdent = do
    ts <- peek
    case underlay <$> ts of
        (Identifier name : _) -> do
            skip 1
            return (head ts $> name)
        (x : _) -> throw (head ts) "Expected identifier here" $ "Got token" +| show x |+ ""
        [] -> do
            sp <- eof
            throw sp "Unexpected end of file" "Expected identifier"
