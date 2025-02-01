{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Asm.Coalescing (coalescing) where

import Asm.Liveness (preColor)
import Asm.Types (AllocM, Neighbour)
import Asm.Utils (k)
import Control.Monad.Reader (ask)
import Control.Monad.State (gets)
import Data.Bifunctor (bimap)
import qualified Data.DisjointSet as DisjointSet
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import IR.Types (IRBlock (IRBlock, insts), IRFunc (IRFunc, blocks), IRInst (Mov), RegImm (TempReg, VarReg))
import Optics (Field1 (_1), Field2 (_2), Field6 (_6), (^.))
import Optics.State.Operators ((%=), (.=))

-- | Find all candidates
coalescingCandidates :: AllocM (Set (Int, Int))
coalescingCandidates = do
    IRFunc{blocks} <- ask
    lookupSet <- gets (^. _6)
    let lookupRep r = fromMaybe (preColor r) $ DisjointSet.representative (preColor r) lookupSet
    return $
        Set.fromList $
            flip concatMap (Map.elems blocks) $
                \IRBlock{insts} -> flip concatMap insts $
                    \case
                        (Mov r1 (VarReg r2)) ->
                            let i1 = lookupRep r1
                                i2 = lookupRep r2
                             in filter (uncurry (/=)) [(max i1 i2, min i1 i2)]
                        (Mov r1 (TempReg r2)) ->
                            let i1 = lookupRep r1
                                i2 = lookupRep r2
                             in filter (uncurry (/=)) [(max i1 i2, min i1 i2)]
                        _ -> []

-- | Check for valid ones, we want to make sure is does not break k-colorable
validCoalescing :: Neighbour -> (Int, Int) -> Bool
validCoalescing n (i1, i2) =
    let findNeighbour i = fromMaybe [] $ Map.lookup i n
        neighbours = findNeighbour i1 `Set.union` findNeighbour i2
        nDegrees = length . findNeighbour <$> Set.toList neighbours
        notNeighbour x y = case Map.lookup x n of
            Just s -> Set.notMember y s
            Nothing -> True
     in notNeighbour i1 i2 && notNeighbour i2 i1 && (length . filter (>= k) $ nDegrees) < k

-- | Repeat merge valid candiates until there is nothing to merge
coalescing :: AllocM ()
coalescing = do
    candidates <- coalescingCandidates
    edges <- gets (^. _2)
    let insert i = Map.alter (Just . maybe [i] (Set.insert i))
        neighbours =
            foldl'
                (\m (i1, i2) -> insert i1 i2 . insert i2 i1 $ m)
                mempty
                $ Set.toList edges
        toCoalescing = filter (validCoalescing neighbours) $ Set.toList candidates
    case toCoalescing of
        ((a, b) : _) -> do
            lookupSetOld <- gets (^. _6)
            let lookupSet = DisjointSet.union a b lookupSetOld
            let replace i = fromMaybe i (DisjointSet.representative i lookupSet)
            _6 .= lookupSet
            _1 %= \graph -> Set.fromList $
                flip concatMap (Set.toList graph) $
                    \x -> case DisjointSet.representative x lookupSet of
                        Just x'
                            | x == x' -> [x]
                            | otherwise -> []
                        Nothing -> [x]
            _2 %= Set.filter (uncurry (/=)) . Set.map (bimap replace replace)
            coalescing
        [] -> return ()
