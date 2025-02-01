{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IR.Types (
    IRReg (..),
    IRInst (..),
    IRJump (..),
    IRBlock (..),
    IRInfo (..),
    IRM,
    GenM,
    IRState (..),
    IR (..),
    FuncType (..),
    IRFunc (..),
    FuncState (..),
    runIRM,
    FallBack (..),
    RegImm (..),
) where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, execStateT)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Error.Diagnose (Position, Report)
import Optics (makeFieldLabelsNoPrefix)
import Optics.Operators ((^.))
import Prettyprinter (Doc, Pretty, pretty, (<+>))
import Source (Source (rest))

data IRReg
    = SpillReg Int
    | Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsi
    | Rdi
    | Rbp
    | Rsp
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Eq, Ord, Show)

instance Pretty IRReg where
    pretty :: IRReg -> Doc ann
    pretty (SpillReg i) = "qword" <+> "[rsp + " <> pretty (i * 8) <> "]"
    pretty Rax = "rax"
    pretty Rbx = "rbx"
    pretty Rcx = "rcx"
    pretty Rdx = "rdx"
    pretty Rsi = "rsi"
    pretty Rdi = "rdi"
    pretty Rbp = "rbp"
    pretty Rsp = "rsp "
    pretty R8 = "r8"
    pretty R9 = "r9"
    pretty R10 = "r10"
    pretty R11 = "r11"
    pretty R12 = "r12"
    pretty R13 = "r13"
    pretty R14 = "r14"
    pretty R15 = "r15"

type SourceLines = [Text]

data RegImm
    = Constant Int
    | TempReg IRReg
    | VarReg IRReg
    deriving (Eq)

instance Pretty RegImm where
    pretty :: RegImm -> Doc ann
    pretty (Constant i) = pretty i
    pretty (TempReg r) = pretty r
    pretty (VarReg r) = pretty r

instance Num RegImm where
    fromInteger :: Integer -> RegImm
    fromInteger = Constant . fromInteger

data IRInst
    = -- Binary op1 <- op2 < op3
      Add IRReg RegImm
    | Sub IRReg RegImm
    | -- | op1 in rax
      Mul IRReg
    | -- | rdx must be cleared, op1 in rax
      Div IRReg
    | Cqo
    | And IRReg RegImm
    | Or IRReg RegImm
    | -- | op2 in rcx
      Shl IRReg
    | -- | op2 in rcx
      Shr IRReg
    | Neg IRReg
    | Not IRReg
    | Cmp IRReg RegImm
    | CMovNZ IRReg IRReg
    | CMovZ IRReg IRReg
    | SetZ IRReg
    | SetNZ IRReg
    | SetG IRReg
    | SetGe IRReg
    | SetE IRReg
    | SetNe IRReg
    | SetL IRReg
    | SetLe IRReg
    | -- | Load address
      La IRReg Text
    | -- | Save to address
      Sa Text RegImm
    | -- | Load symbol value
      Lsv IRReg Text
    | -- | Load byte r0 <- r0[r1]
      Lb IRReg RegImm
    | -- | Store byte r1[r2*size] <- r0
      Sb RegImm IRReg RegImm
    | -- | Load qword r0 <- r0[r1]
      Lqw IRReg RegImm
    | -- | Store qword r1[r2*size] <- r0
      Sqw RegImm IRReg RegImm
    | -- | op1 <- op2
      Mov IRReg RegImm
    | Call Text
    | Push IRReg
    | Pop IRReg
    | IRComment Position SourceLines
    deriving (Eq)

data IRJump
    = Jmp Int
    | Branch RegImm Int Int
    | Ret
    deriving (Eq)

data IRBlock = IRBlock
    { label :: Int
    -- ^ Label for the block
    , label2 :: Maybe Text
    -- ^ Label if this is entry of a function
    , insts :: [IRInst]
    -- ^ All instructions
    , flow :: IRJump
    -- ^ Edge to next IRBlock
    }
    deriving (Eq)
makeFieldLabelsNoPrefix ''IRBlock

data IRFunc = IRFunc
    { name :: Text
    , params :: [IRReg]
    , regMap :: Map Int (Maybe Text)
    , root :: Int
    , blocks :: Map Int IRBlock
    , stackSize :: Int
    , usedReg :: Set IRReg
    }
    deriving (Eq)
makeFieldLabelsNoPrefix ''IRFunc

data FuncType = IRRegular | VoidMain | MainArg

data IR = IR
    { externs :: Set Text
    , globalVars :: Map Text Int
    , strings :: Map Int Text
    , funcs :: Map Text IRFunc
    }
    deriving (Eq)
makeFieldLabelsNoPrefix ''IR

data FallBack
    = Goto Int
    | RetVoid
    deriving (Show)

data FuncState = FuncState
    { regMap :: Map Int (Maybe Text)
    , fallBack :: FallBack
    , blockPosition :: Position
    , blocks :: Map Int IRBlock
    }
makeFieldLabelsNoPrefix ''FuncState

data IRState = IRState
    { nameMap :: Map Text (Either IRReg Text)
    , nextBlockLabel :: Int
    , nextStrLabel :: Int
    , ir :: IR
    }
makeFieldLabelsNoPrefix ''IRState

newtype IRInfo = IRInfo
    { source :: SourceLines
    }
makeFieldLabelsNoPrefix ''IRInfo

type GenM s a = StateT s (ReaderT IRInfo (Except (Position, Report Text))) a
type IRM a = GenM IRState a

runIRM :: Source -> (b -> IRM a) -> b -> Either (Position, Report Text) IR
runIRM src m b =
    let m'' = runReaderT m' (IRInfo $ Text.lines $ Text.pack src.rest)
        m' = execStateT (m b) (IRState mempty 0 0 (IR mempty mempty mempty mempty))
     in (^. #ir) <$> runExcept m''
