{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Asm.RegSpill (spill) where

import Asm.Utils (mov8Bit, spillR, spillRM, spillRMI, spillRMI8bit)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Map (assocs, (!))
import qualified Data.Map as Map
import Data.Set (elems)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Error.Diagnose (Position (Position))
import Fmt ((+|), (|+))
import IR.Simplify (mendCallRets, simplify)
import IR.Types (IR (..), IRBlock (..), IRFunc (..), IRInst (..), IRJump (Branch, Jmp, Ret), IRReg (..), RegImm (..))
import Prettyprinter (Doc, fill, indent, line, pretty, vsep, (<+>))

spill :: IR -> Doc ann
spill = spill' . simplify . mendCallRets

spill' :: IR -> Doc ann
spill' IR{..} =
    indent
        4
        ( vsep $
            map (\t -> pretty @Text $ "extern " +| t |+ "") $
                elems $
                    ["malloc", "free"] <> externs
        )
        <> line
        <> indent 4 "global main"
        <> line
        <> line
        <> indent 4 "section .data"
        <> line
        <> vsep
            ( flip map (assocs strings) $ \(label, text) ->
                pretty @Text ("str_" +| label |+ ":")
                    <> line
                    <> indent
                        4
                        ( fill 8 "db"
                            <+> pretty (intercalate "," (map (show . ord) (unpack text) ++ ["0"]))
                        )
            )
        <> line
        <> vsep
            ( flip map (assocs globalVars) $ \(label, i) ->
                pretty @Text ("" +| label |+ ":") <> line <> indent 4 (fill 8 "dq" <+> pretty i)
            )
        <> line
        <> line
        <> indent 4 "section .text"
        <> line
        <> line
        <> vsep (spillFunc <$> Map.elems funcs)

spillFunc :: IRFunc -> Doc ann
spillFunc f@IRFunc{blocks} =
    vsep (spillBlock f <$> Map.elems blocks)

spillBlock :: IRFunc -> IRBlock -> Doc ann
spillBlock f IRBlock{..} =
    displayLabel f label
        <> ":"
        <> line
        <> indent 4 (vsep $ map spillInst insts ++ [spillFlow f flow])
        <> line

displayLabel :: IRFunc -> Int -> Doc ann
displayLabel IRFunc{name, blocks} blockId =
    case label2 of
        Just fname -> pretty fname
        Nothing -> pretty @Text ("" +| name |+ "_" +| label |+ "")
  where
    IRBlock{..} = blocks ! blockId

spillInst :: IRInst -> Doc ann
spillInst (Add reg regImm) = spillRM regImm Rbx reg $ \regImm' ->
    fill 8 "add" <+> pretty reg <> "," <+> regImm'
spillInst (Sub reg regImm) = spillRM regImm Rbx reg $ \regImm' ->
    fill 8 "sub" <+> pretty reg <> "," <+> regImm'
spillInst (Mul reg) = fill 8 "imul" <+> pretty reg
spillInst (Div reg) = fill 8 "idiv" <+> pretty reg
spillInst Cqo = "cqo"
spillInst (And reg regImm) = spillRM regImm Rbx reg $ \regImm' ->
    fill 8 "and" <+> pretty reg <> "," <+> regImm'
spillInst (Or reg regImm) = spillRM regImm Rbx reg $ \regImm' ->
    fill 8 "or" <+> pretty reg <> "," <+> regImm'
spillInst (Shl reg) = fill 8 "shl" <+> pretty reg <> "," <+> "cl"
spillInst (Shr reg) = fill 8 "shr" <+> pretty reg <> "," <+> "cl"
spillInst (Not reg) = fill 8 "not" <+> pretty reg
spillInst (Neg reg) = fill 8 "neg" <+> pretty reg
spillInst (Cmp reg regImm) = spillRM regImm Rbx reg $ \regImm' ->
    fill 8 "cmp" <+> pretty reg <> "," <+> regImm'
spillInst (CMovNZ reg1 reg2) = spillR reg1 Rbx $ \reg1' ->
    fill 8 "cmovnz" <+> pretty reg1' <> "," <+> pretty reg2
spillInst (CMovZ reg1 reg2) = spillR reg1 Rbx $ \reg1' ->
    fill 8 "cmovz" <+> pretty reg1' <> "," <+> pretty reg2
spillInst (SetLe reg) = spillR reg Rbx $ \reg' ->
    fill 8 "setle" <+> pretty reg'
spillInst (SetGe reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "setge" <+> reg'
spillInst (SetL reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "setl" <+> reg'
spillInst (SetG reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "setg" <+> reg'
spillInst (SetNe reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "setne" <+> reg'
spillInst (SetE reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "sete" <+> reg'
spillInst (SetZ reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "setz" <+> reg'
spillInst (SetNZ reg) = mov8Bit reg Rbx $ \reg' ->
    fill 8 "setnz" <+> reg'
spillInst (Mov reg regImm) = spillRM regImm Rbx reg $ \regImm' ->
    fill 8 "mov" <+> pretty reg <> "," <+> regImm'
spillInst (La reg addr) = fill 8 "mov" <+> pretty reg <> "," <+> pretty addr
spillInst (Sa addr reg@(TempReg (SpillReg _))) =
    vsep
        [ fill 8 "mov" <+> "rax" <> "," <+> pretty reg
        , fill 8 "mov" <+> "[" <> pretty addr <> "]" <> "," <+> "rax"
        ]
spillInst (Sa addr reg@(VarReg (SpillReg _))) =
    vsep
        [ fill 8 "mov" <+> "rax" <> "," <+> pretty reg
        , fill 8 "mov" <+> "[" <> pretty addr <> "]" <> "," <+> "rax"
        ]
spillInst (Sa addr reg) =
    fill 8 "mov" <+> "[" <> pretty addr <> "]" <> "," <+> pretty reg
spillInst (Lb r0 offset) = spillR r0 Rbx $
    \r0' -> spillRMI offset R11 $ \offset' ->
        fill 8 "movsx" <+> pretty r0' <> "," <+> "byte" <+> "[" <> pretty r0' <+> "+" <+> offset' <> "]"
spillInst (Lqw r0 offset) = spillR r0 Rbx $ \r0' -> spillRMI offset R11 $ \offset' ->
    fill 8 "mov" <+> pretty r0' <> "," <+> "qword" <+> "[" <> pretty r0' <+> "+" <+> offset' <+> "* 8" <> "]"
spillInst (Sb r0 r1 offset) = spillR r1 Rbx $
    \r1' -> spillRMI8bit r0 R11 $
        \r0' -> spillRMI offset R10 $
            \offset' ->
                fill 8 "mov" <+> "byte" <+> "[" <> pretty r1' <+> "+" <+> offset' <> "]" <> "," <+> r0'
spillInst (Sqw r0 r1 offset) = spillR r1 Rbx $
    \r1' -> spillRMI r0 R11 $
        \r0' -> spillRMI offset R10 $
            \offset' ->
                fill 8 "mov" <+> "qword" <+> "[" <> pretty r1' <+> "+" <+> offset' <+> "* 8" <> "]" <> "," <+> r0'
spillInst (Lsv reg addr) = spillR reg Rbx $ \reg' ->
    fill 8 "mov" <+> pretty reg' <> "," <+> "[" <> pretty addr <> "]"
spillInst (Call "printf") =
    vsep
        [ fill 8 "mov" <+> "rax" <> "," <+> "0"
        , fill 8 "call" <+> "printf"
        ]
spillInst (Call t) = fill 8 "call" <+> "$" <> pretty t
spillInst (Push p) = fill 8 "push" <+> pretty p
spillInst (Pop p) = fill 8 "pop" <+> pretty p
spillInst (IRComment p@(Position (ln, col) _ _) src) =
    let t = Text.drop (col - 1) $ src !! (ln - 1)
     in "; " <+> pretty p <> line <> "; " <+> pretty t

spillFlow :: IRFunc -> IRJump -> Doc ann
spillFlow _ Ret =
    fill 8 "ret"
spillFlow f (Jmp b) =
    fill 8 "jmp" <+> displayLabel f b
spillFlow f (Branch (TempReg reg) bt bf) =
    vsep
        [ fill 8 "cmp" <+> pretty reg <> "," <+> "0"
        , fill 8 "je" <+> displayLabel f bf
        , fill 8 "jmp" <+> displayLabel f bt
        ]
spillFlow f (Branch (VarReg reg) bt bf) =
    vsep
        [ fill 8 "cmp" <+> pretty reg <> "," <+> "0"
        , fill 8 "je" <+> displayLabel f bf
        , fill 8 "jmp" <+> displayLabel f bt
        ]
spillFlow f (Branch (Constant i) bt bf)
    | i == 0 =
        fill 8 "jmp" <+> displayLabel f bf
    | otherwise = fill 8 "jmp" <+> displayLabel f bt
