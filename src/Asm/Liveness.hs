{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Asm.Liveness (LiveNode (..), analysisIR, prettyIRLiveness, analysisFunc, colorToReg, preColor) where

import Data.Foldable (foldl')
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Fmt (padLeftF, (+|), (|+))
import IR.Types (IR (..), IRBlock (..), IRFunc (..), IRInst (..), IRJump (..), IRReg (..), RegImm (..))
import Optics (makeFieldLabelsNoPrefix, (^.))
import Prettyprinter (Doc, Pretty (pretty), encloseSep, fill, indent, line, vsep)
import IR.Simplify (callerSaved)

fromIRReg :: IRReg -> Set Int
fromIRReg (SpillReg i) = [i]
fromIRReg r = [preColor r]

fromRegImm :: RegImm -> Set Int
fromRegImm (VarReg r) = fromIRReg r
fromRegImm (TempReg r) = fromIRReg r
fromRegImm _ = []

preColor :: IRReg -> Int
preColor reg =
    case reg of
        Rax -> -1
        Rcx -> -3
        Rdx -> -4
        Rsi -> -5
        Rdi -> -6
        Rbp -> -7
        R8 -> -9
        R9 -> -10
        R12 -> -13
        R13 -> -14
        R14 -> -15
        R15 -> -16
        (SpillReg i) -> i
        _ -> undefined

colorToReg :: Int -> IRReg
colorToReg i =
    case i of
        -1 -> Rax
        -3 -> Rcx
        -4 -> Rdx
        -5 -> Rsi
        -6 -> Rdi
        -7 -> Rbp
        -9 -> R8
        -10 -> R9
        -13 -> R12
        -14 -> R13
        -15 -> R14
        -16 -> R15
        _ -> undefined

data LiveNode = LiveNode
    { name :: Text
    , succs :: Set Text
    , def :: Set Int
    , use :: Set Int
    , liveIn :: Set Int
    , liveOut :: Set Int
    }
    deriving (Eq)
makeFieldLabelsNoPrefix ''LiveNode

newNode :: Text -> Set Text -> IRInst -> LiveNode
newNode name succs inst =
    LiveNode{liveIn = mempty, liveOut = mempty, ..}
  where
    (def, use) = case inst of
        Add r0 r1 -> (fromIRReg r0, fromIRReg r0 `Set.union` fromRegImm r1)
        Sub r0 r1 -> (fromIRReg r0, fromIRReg r0 `Set.union` fromRegImm r1)
        Mul r0 -> (Set.map preColor [Rax, Rdx], fromIRReg r0 `Set.union` [preColor Rax])
        Div r0 -> (Set.map preColor [Rax, Rdx], fromIRReg r0 `Set.union` Set.map preColor [Rax, Rdx])
        Cqo -> ([preColor Rdx], [preColor Rax])
        And r0 r1 -> (fromIRReg r0, fromIRReg r0 `Set.union` fromRegImm r1)
        Or r0 r1 -> (fromIRReg r0, fromIRReg r0 `Set.union` fromRegImm r1)
        Shl r0 -> (fromIRReg r0, fromIRReg r0 `Set.union` [preColor Rcx])
        Shr r0 -> (fromIRReg r0, fromIRReg r0 `Set.union` [preColor Rcx])
        Neg r0 -> (fromIRReg r0, fromIRReg r0)
        Not r0 -> (fromIRReg r0, fromIRReg r0)
        Cmp r0 r1 -> ([], fromIRReg r0 `Set.union` fromRegImm r1)
        CMovNZ r0 r1 -> (fromIRReg r0, fromIRReg r1)
        CMovZ r0 r1 -> (fromIRReg r0, fromIRReg r1)
        SetZ r0 -> (fromIRReg r0, [])
        SetNZ r0 -> (fromIRReg r0, [])
        SetG r0 -> (fromIRReg r0, [])
        SetGe r0 -> (fromIRReg r0, [])
        SetE r0 -> (fromIRReg r0, [])
        SetNe r0 -> (fromIRReg r0, [])
        SetL r0 -> (fromIRReg r0, [])
        SetLe r0 -> (fromIRReg r0, [])
        La r0 _ -> (fromIRReg r0, [])
        Sa _ r0 -> ([], fromRegImm r0)
        Lsv r0 _ -> (fromIRReg r0, [])
        Lb r0 r1 -> (fromIRReg r0, fromIRReg r0 `Set.union` fromRegImm r1)
        Sb r0 r1 r2 -> ([], fromRegImm r0 `Set.union` fromIRReg r1 `Set.union` fromRegImm r2)
        Lqw r0 r1 -> (fromIRReg r0, fromIRReg r0 `Set.union` fromRegImm r1)
        Sqw r0 r1 r2 -> ([], fromRegImm r0 `Set.union` fromIRReg r1 `Set.union` fromRegImm r2)
        Mov r0 r1 -> (fromIRReg r0, fromRegImm r1)
        Push _ -> ([], [])
        Pop _ -> ([], [])
        Call _ -> (Set.map preColor callerSaved, [])
        IRComment _ _ -> ([], [])

analysisFunc :: IRFunc -> Map Text LiveNode
analysisFunc IRFunc{blocks} =
    let nodes = fromBlock <$> Map.elems blocks
     in resolveGraph $ foldl' Map.union mempty nodes

analysisIR :: IR -> Map Text (Map Text LiveNode)
analysisIR IR{..} =
    Map.map analysisFunc funcs

prettyLiveNode :: Map Int (Maybe Text) -> LiveNode -> Doc ann
prettyLiveNode m LiveNode{..} =
    fill 15 (pretty name <> ":")
        <> line
        <> indent
            8
            ( vsep
                [ fill 8 "succ" <> prettySet succs
                , fill 8 "def" <> prettySet (nameRegs def)
                , fill 8 "use" <> prettySet (nameRegs use)
                , fill 8 "in" <> prettySet (nameRegs liveIn)
                , fill 8 "out" <> prettySet (nameRegs liveOut)
                ]
            )
  where
    prettySet s = encloseSep "{" "}" ", " $ pretty <$> Set.toList s
    nameRegs = Set.map $ \i ->
        if i >= 0 then
            case m ! i of
                Just t -> t
                Nothing -> "tmp_" +| i |+ ""
        else
            pack . show . pretty $ colorToReg i

prettyIRLiveness :: IR -> Map Text (Map Text LiveNode) -> Doc ann
prettyIRLiveness IR{funcs} m =
    vsep $ flip map blocks $ \(f, g) ->
        pretty f
            <> ":"
            <> line
            <> indent 4 (vsep $ prettyBlock f <$> Map.elems g)
            <> line
  where
    blocks = Map.toList m
    prettyBlock fname = prettyLiveNode (funcs ! fname ^. #regMap)

nodeName :: Int -> Int -> Text
nodeName b n = "b" +| padLeftF 3 '0' b |+ "_i" +| padLeftF 3 '0' n |+ ""

fromBlock :: IRBlock -> Map Text LiveNode
fromBlock IRBlock{..} =
    let names = nodeName label <$> [1 .. length insts + 1]
        succs = Set.singleton <$> tail names
        nodes = flip map (zip insts (zip names succs)) $ \(inst, (name, succ')) ->
            newNode name succ' inst
        nodeLast =
            LiveNode
                { name = last names
                , succs = case flow of
                    Jmp b -> [nodeName b 1]
                    Branch _ b1 b2 -> [nodeName b1 1, nodeName b2 1]
                    Ret -> []
                , def = mempty
                , use = case flow of
                    Jmp _ -> []
                    Branch reg _ _ -> fromRegImm reg
                    Ret -> []
                , liveIn = mempty
                , liveOut = mempty
                }
     in Map.fromList $ (\node -> (node ^. #name, node)) <$> nodes ++ [nodeLast]

resolveGraph :: Map Text LiveNode -> Map Text LiveNode
resolveGraph m =
    let m' = updateNode <$> m
     in if m == m' then m else resolveGraph m'
  where
    updateNode LiveNode{..} =
        let allSuccs = (\n -> m ! n ^. #liveIn) <$> Set.toDescList succs
            liveOut' = foldl' Set.union mempty allSuccs
            liveIn' = Set.union use (liveOut' \\ def)
         in LiveNode
                { liveOut = liveOut'
                , liveIn = liveIn'
                , ..
                }
