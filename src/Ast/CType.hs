{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast.CType (CType (..)) where

import Ast.Ast (Ast, IsAst, Phase, underlay)
import Ast.ShowText (showText)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty, pretty)

data CType (p :: Phase)
    = TVoid
    | TInt
    | TChar
    | TIdent Text
    | TPoint (Ast p (CType p))

deriving instance Eq (Ast p (CType p)) => Eq (CType p)

instance (IsAst (Ast a (CType a)) (CType a)) => Pretty (CType a) where
    pretty :: CType a -> Doc ann
    pretty t = case t of
        TVoid -> "TVoid"
        TInt -> "TInt"
        TChar -> "TChar"
        TIdent s -> "TIdent" <> "(" <> showText s <> ")"
        TPoint ty -> "TPoint" <> "(" <> pretty (underlay @_ @(CType a) ty) <> ")"

instance (IsAst (Ast a (CType a)) (CType a)) => Show (CType a) where
    show :: CType a -> String
    show = show . pretty
