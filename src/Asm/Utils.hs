{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Utils (spillRM, spillRMI, spillR, mov8Bit, spillRMI8bit, k) where

import IR.Types (IRReg (..), RegImm (..))
import Prettyprinter (Doc, fill, pretty, vsep, (<+>))
import IR.Simplify (safeRegisters)

spillR :: IRReg -> IRReg -> (IRReg -> Doc ann) -> Doc ann
spillR src@(SpillReg _) tar f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f tar
        , fill 8 "mov" <+> pretty src <> "," <+> pretty tar
        ]
spillR reg _ f = f reg

-- | Spill on RM instruction, bool :: write back
spillRM :: RegImm -> IRReg -> IRReg -> (Doc ann -> Doc ann) -> Doc ann
spillRM src@(TempReg (SpillReg _)) tar (SpillReg _) f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ pretty tar
        ]
spillRM src@(VarReg (SpillReg _)) tar (SpillReg _) f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ pretty tar
        ]
spillRM reg _ _ f = f $ pretty reg

spillRMI :: RegImm -> IRReg -> (Doc ann -> Doc ann) -> Doc ann
spillRMI (TempReg src@(SpillReg _)) tar f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ pretty tar
        ]
spillRMI (VarReg src@(SpillReg _)) tar f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ pretty tar
        ]
spillRMI src _ f = f $ pretty src

spillRMI8bit :: RegImm -> IRReg -> (Doc ann -> Doc ann) -> Doc ann
spillRMI8bit (TempReg src@(SpillReg _)) tar f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ to8Bit tar
        ]
spillRMI8bit (TempReg src) _ f = f $ to8Bit src
spillRMI8bit (VarReg src@(SpillReg _)) tar f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ to8Bit tar
        ]
spillRMI8bit (VarReg src) _ f = f $ to8Bit src
spillRMI8bit src _ f = f $ pretty src

to8Bit :: IRReg -> Doc ann
to8Bit (SpillReg _) = error "Unreachable"
to8Bit Rax = "al"
to8Bit Rbx = "bl"
to8Bit Rcx = "cl"
to8Bit Rdx = "dl"
to8Bit Rsi = "sil"
to8Bit Rdi = "dil"
to8Bit Rbp = "bpl"
to8Bit Rsp = "spl"
to8Bit R8 = "r8b"
to8Bit R9 = "r9b"
to8Bit R10 = "r10b"
to8Bit R11 = "r11b"
to8Bit R12 = "r12b"
to8Bit R13 = "r13b"
to8Bit R14 = "r14b"
to8Bit R15 = "r15b"

mov8Bit :: IRReg -> IRReg -> (Doc ann -> Doc ann) -> Doc ann
mov8Bit src@(SpillReg _) tar f =
    vsep
        [ fill 8 "mov" <+> pretty tar <> "," <+> pretty src
        , f $ to8Bit tar
        , fill 8 "movsx" <+> pretty tar <> "," <+> to8Bit tar
        , fill 8 "mov" <+> pretty src <> "," <+> pretty tar
        ]
mov8Bit src _ f =
    vsep
        [ f $ to8Bit src
        , fill 8 "movsx" <+> pretty src <> "," <+> to8Bit src
        ]

k :: Int
k = length safeRegisters
