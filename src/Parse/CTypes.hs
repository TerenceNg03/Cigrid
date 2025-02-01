{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.CTypes (typeP) where

import Ast.Ast (Ast, Phase (Parsed), underlay)
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.CType (CType (..))
import Ast.Token (Token (..))
import Data.Functor (($>))
import Parse.Types (ParseM)
import Parse.Utils (eof, peek, skip, throw)
import Source (HasPosition (pos), (<->))
import Prelude hiding (lex)

typeP :: ParseM (Ast Parsed (CType Parsed))
typeP = do
    ts <- peek
    ty <- case underlay <$> ts of
        (Void : _) -> skip 1 >> return (head ts $> TVoid)
        (IntT : _) -> skip 1 >> return (head ts $> TInt)
        (CharT : _) -> skip 1 >> return (head ts $> TChar)
        (Identifier t : _) -> skip 1 >> return (head ts $> TIdent t)
        (_ : _) -> throw (pos $ head ts) "Invalid type" "here"
        [] -> do
            sp <- eof
            throw sp "Unexpected end of file" "Expected type"
    parsePtr ty
  where
    parsePtr :: Ast Parsed (CType Parsed) -> ParseM (Ast Parsed (CType Parsed))
    parsePtr ty = do
        ts <- peek
        case underlay <$> ts of
            (Mul : _) -> do
                skip 1
                let ty' = AstSrc (ty <-> head ts) $ TPoint ty
                parsePtr ty'
            _ -> return ty
