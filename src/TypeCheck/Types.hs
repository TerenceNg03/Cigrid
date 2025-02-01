{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TypeCheck.Types (CiType (..), TypeM, HasType (..), TypeState (..), AstTyped (..), runType) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Typed))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, evalStateT)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Error.Diagnose (Position, Report)
import Fmt ((+|), (|+))
import NameAnalysis.Types ()
import Source (HasPosition (pos))

type instance Ast Typed a = AstTyped a

data AstTyped a = AstTyped Position CiType a

class HasType a where
    getType :: a -> CiType

instance HasType (AstTyped a) where
    getType :: AstTyped a -> CiType
    getType (AstTyped _ t _) = t

instance HasType CiType where
    getType :: CiType -> CiType
    getType = id

instance (Show a) => Show (AstTyped a) where
    show :: AstTyped a -> String
    show (AstTyped _ _ x) = show x

instance IsAst (AstTyped a) a where
    underlay :: AstTyped a -> a
    underlay (AstTyped _ _ x) = x

instance (Eq a) => Eq (AstTyped a) where
    (==) :: AstTyped a -> AstTyped a -> Bool
    (AstTyped _ _ x) == (AstTyped _ _ y) = x == y

instance Functor AstTyped where
    fmap :: (a -> b) -> AstTyped a -> AstTyped b
    fmap f (AstTyped sp ty x) = AstTyped sp ty $ f x

instance HasPosition (AstTyped a) where
    pos :: AstTyped a -> Position
    pos (AstTyped sp _ _) = sp

data CiType
    = Void
    | -- | Bool for isZero
      Int Bool
    | Char
    | Ptr CiType
    | Struct Text
    | Arrow CiType CiType

instance Eq CiType where
    (==) :: CiType -> CiType -> Bool
    Void == Void = True
    Int _ == Int _ = True
    Char == Int _ = True
    Int _ == Char = True
    Char == Char = True
    Ptr x == Ptr y = x == y
    Ptr _ == Int True = True
    Struct x == Struct y = x == y
    Arrow x1 x2 == Arrow y1 y2 = x1 == y1 && x2 == y2
    _ == _ = False
instance Show CiType where
    show :: CiType -> String
    show Void = "Void"
    show (Int False) = "Int"
    show (Int True) = "Int | Ptr a"
    show Char = "Char"
    show (Ptr t) = "Ptr (" +| show t |+ ")"
    show (Struct t) = "Struct " +| t |+ ""
    show (Arrow t1 t2) = "" +| show t1 |+ " -> " +| show t2 |+ ""

data TypeState = TypeState
    { names :: Map Text CiType
    , structFields :: Map (Text, Text) CiType
    , structs :: Set Text
    , canBreak :: Bool
    , returnType :: CiType
    }

type TypeM a = StateT TypeState (Except (Position, Report Text)) a

runType :: (b -> TypeM a) -> b -> Either (Position, Report Text) a
runType m b =
    let state = TypeState mempty mempty mempty False Void
     in runExcept $ evalStateT (m b) state
