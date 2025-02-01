{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Asm.RegAlloc (allocReg, prettyIRInterGraph, AllocM) where

import Asm.Coalescing (coalescing)
import Asm.Liveness (LiveNode (LiveNode, def, liveOut), analysisFunc, colorToReg, preColor)
import Asm.Types (Alloc, AllocM, AllocType (..), Edges, runGraph)
import Asm.Utils (k)
import Control.Exception (assert)
import Control.Monad.State (gets)
import qualified Data.DisjointSet as DisjointSet
import Data.Foldable (foldl', forM_)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Fmt ((+|), (|+))
import IR.Simplify (safeRegisters)
import IR.Types (IR (IR), IRBlock (..), IRFunc (..), IRInst (..), IRJump (..), IRReg (SpillReg), RegImm (..), funcs)
import Optics (Field1 (_1), Field2 (_2), Field3 (_3), Field4 (_4), Field5 (_5), Field6 (_6), (%~), (^.))
import Optics.State.Operators ((%=), (.=))
import Prettyprinter (Doc, indent, line, pretty, vsep, (<+>))

prettyIRInterGraph :: IR -> Doc ann
prettyIRInterGraph IR{funcs} =
    "graph"
        <+> "G"
        <+> "{"
        <> line
        <> indent
            4
            (vsep $ prettyInterGraph <$> Map.elems funcs)
        <> "}"
        <> line

prettyInterGraph :: IRFunc -> Doc ann
prettyInterGraph f@IRFunc{name, regMap} =
    prettyGraph
  where
    nodes = analysisFunc f
    edges = Set.fromList $
        flip concatMap (Map.elems nodes) $
            \LiveNode{def, liveOut} ->
                [ if a > b then (a, b) else (b, a)
                | a <- Set.toList def
                , b <- Set.toList liveOut
                , a /= b
                ]
    nameReg i
        | i >= 0 =
            assert (isJust $ Map.lookup i regMap) $
                case regMap ! i of
                    Just t -> pretty $ Text.replace "." "_" t
                    Nothing -> pretty @Text $ "" +| name |+ "_" +| i |+ ""
        | otherwise = pretty $ colorToReg i
    prettyGraph =
        "subgraph"
            <+> pretty name
            <+> "{"
            <> line
            <> indent
                4
                (vsep ((\(x, y) -> nameReg x <+> "--" <+> nameReg y) <$> Set.toList edges))
            <> line
            <> "}"
            <> line

processNode :: AllocM ()
processNode = do
    nodes <- gets (^. _1)
    case Set.lookupGE 0 nodes of
        Just node -> do
            edges <- gets (Set.filter (\(a, b) -> a == node || b == node) . (^. _2))
            let edges' = Set.map (\(a, b) -> (max a b, min a b)) edges
            _1 %= Set.delete node
            _2 %= (\\ edges)
            case Set.size edges' of
                x
                    | x < k -> _3 %= ((node, edges, Color) :)
                    | otherwise -> _3 %= ((node, edges, Spill) :)
            processNode
        Nothing -> return ()

simplifyGraph :: AllocM ()
simplifyGraph = processNode

-- | Color all registes that are coalesced
assignNodes :: Int -> IRReg -> AllocM ()
assignNodes node color = do
    lookupSet <- gets (^. _6)
    let assigns = Set.toList $ DisjointSet.equivalences node lookupSet
        assignsMap = Map.fromList $ (,color) <$> assigns
    _4 %= Map.union assignsMap

-- | Color all physical register that is used
assignPreColor :: AllocM ()
assignPreColor = forM_ (Set.toList safeRegisters) $ \reg -> do
    lookupSet <- gets (^. _6)
    case DisjointSet.representative (preColor reg) lookupSet of
        Just _ -> assignNodes (preColor reg) reg
        _ -> return ()

-- | Color virtual registers
assignNormalColor :: AllocM ()
assignNormalColor = do
    stack <- gets (^. _3)
    assigned <- gets (^. _4)
    case stack of
        [] -> return ()
        -- Do not color is already colored
        ((node, _, _) : xs)
            | isJust (Map.lookup node assigned) -> do
                _3 .= xs
                assignNormalColor
        ((node, edges, Color) : xs) | node >= 0 -> do
            colors <- neighbour edges node
            let color = head $ Set.toList $ safeRegisters \\ colors
            assignNodes node color
            _3 .= xs
            assignNormalColor
        ((node, edges, Spill) : xs) | node >= 0 -> do
            colors <- neighbour edges node
            let colors' = Set.toList $ safeRegisters \\ colors
            case colors' of
                [] -> do
                    idx <- gets (^. _5)
                    _5 %= (+ 1)
                    assignNodes node (SpillReg idx)
                (x : _) -> assignNodes node x
            _3 .= xs
            assignNormalColor
        (_ : xs) -> do
            _3 .= xs
            assignNormalColor
  where
    neighbour :: Edges -> Int -> AllocM (Set IRReg)
    neighbour edges node = do
        assigned <- gets (^. _4)
        let neighbours = (\(a, b) -> if a == node then b else a) <$> Set.toList edges
            n1 = map (\x -> fromJust $ Map.lookup x assigned) . filter (>= 0) $ neighbours
            n2 = map colorToReg . filter (< 0) $ neighbours
        return $ Set.fromList (n1 ++ n2)

assignColor :: AllocM ()
assignColor = assignPreColor >> assignNormalColor

-- | Replace virtual register with real registers
mendInst :: Alloc -> IRInst -> IRInst
mendInst allocs inst =
    case inst of
        Add r0 r1 -> Add (allocR r0) (allocRI r1)
        Sub r0 r1 -> Sub (allocR r0) (allocRI r1)
        Mul r0 -> Mul $ allocR r0
        Div r0 -> Div $ allocR r0
        Cqo -> Cqo
        And r0 r1 -> And (allocR r0) (allocRI r1)
        Or r0 r1 -> Or (allocR r0) (allocRI r1)
        Shl r0 -> Shl $ allocR r0
        Shr r0 -> Shr $ allocR r0
        Neg r0 -> Neg $ allocR r0
        Not r0 -> Not $ allocR r0
        Cmp r0 r1 -> Cmp (allocR r0) (allocRI r1)
        CMovNZ r0 r1 -> CMovNZ (allocR r0) (allocR r1)
        CMovZ r0 r1 -> CMovZ (allocR r0) (allocR r1)
        SetZ r0 -> SetZ $ allocR r0
        SetNZ r0 -> SetNZ $ allocR r0
        SetG r0 -> SetG $ allocR r0
        SetGe r0 -> SetGe $ allocR r0
        SetE r0 -> SetE $ allocR r0
        SetNe r0 -> SetNe $ allocR r0
        SetL r0 -> SetL $ allocR r0
        SetLe r0 -> SetLe $ allocR r0
        La r0 t -> La (allocR r0) t
        Sa t r0 -> Sa t (allocRI r0)
        Lsv r0 t -> Lsv (allocR r0) t
        Lb r0 r1 -> Lb (allocR r0) (allocRI r1)
        Sb r0 r1 r2 -> Sb (allocRI r0) (allocR r1) (allocRI r2)
        Lqw r0 r1 -> Lqw (allocR r0) (allocRI r1)
        Sqw r0 r1 r2 -> Sqw (allocRI r0) (allocR r1) (allocRI r2)
        Mov r0 r1 -> Mov (allocR r0) (allocRI r1)
        Call t -> Call t
        Push p -> Push p
        Pop p -> Pop p
        IRComment x y -> IRComment x y
  where
    allocRI = allocRI' allocs
    allocR = allocR' allocs

-- | Replace register in flow control and remove useless mov
mendBlock :: Alloc -> IRBlock -> IRBlock
mendBlock allocs b@IRBlock{insts, flow} =
    b{insts = coalesce $ mendInst allocs <$> insts, flow = flow'}
  where
    flow' = case flow of
        Jmp _ -> flow
        Branch reg x y -> Branch (allocRI reg) x y
        Ret -> flow
    allocRI = allocRI' allocs
    coalesce (Mov r0 (TempReg r1) : xs) | r0 == r1 = coalesce xs
    coalesce (Mov r0 (VarReg r1) : xs) | r0 == r1 = coalesce xs
    coalesce (x : xs) = x : coalesce xs
    coalesce [] = []

allocR' :: Alloc -> IRReg -> IRReg
allocR' allocs (SpillReg i) =
    case Map.lookup i allocs of
        Just reg -> reg
        Nothing -> SpillReg i
allocR' _ reg = reg

allocRI' :: Alloc -> RegImm -> RegImm
allocRI' allocs (VarReg reg) = VarReg $ allocR' allocs reg
allocRI' allocs (TempReg reg) = TempReg $ allocR' allocs reg
allocRI' _ (Constant i) = Constant i

allocFunc :: IRFunc -> IRFunc
allocFunc f@IRFunc{blocks, params} =
    f
        { blocks = blocks'
        , stackSize = stackSize
        , params = params'
        , usedReg = usedReg
        }
  where
    allocs = runGraph f (coalescing >> simplifyGraph >> assignColor)
    blocks' = Map.map (mendBlock allocs) blocks
    params' = allocR' allocs <$> params
    stackSize = foldl' max 0 $ (\case SpillReg i -> (i + 1) * 8; _ -> 0) <$> Map.elems allocs
    usedReg = Set.fromList $ Map.elems allocs

allocReg :: IR -> IR
allocReg = #funcs %~ Map.map allocFunc
