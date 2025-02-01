{-# LANGUAGE NamedFieldPuns #-}

module Asm.Types (
    Neighbour,
    AllocM,
    Edges,
    AllocType (..),
    Stack,
    Graph,
    Alloc,
    runGraph,
) where

import Asm.Liveness (LiveNode (LiveNode, def, liveOut, use), analysisFunc)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (State, execState)
import Data.DisjointSet (DisjointSet)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IR.Types (IRReg, IRFunc)
import Optics (Field4 (_4), (^.))

type Edges = Set (Int, Int)
data AllocType = Color | Spill
    deriving (Show)
type Stack = [(Int, Edges, AllocType)]
type Graph = Set Int
type Alloc = Map Int IRReg

type Neighbour = Map Int (Set Int)
type AllocM a = ReaderT IRFunc (State (Graph, Edges, Stack, Alloc, Int, DisjointSet Int)) a

runGraph :: IRFunc -> AllocM a -> Alloc
runGraph f m =
    execState (runReaderT m f) (graph, edges, [], mempty, 0, mempty) ^. _4
  where
    nodes = analysisFunc f
    graph =
        Set.fromList $
            flip concatMap (Map.elems nodes) $
                \LiveNode{def, use} -> Set.toList def ++ Set.toList use
    edges = Set.fromList $
        flip concatMap (Map.elems nodes) $
            \LiveNode{def, liveOut} ->
                [ if a > b then (a, b) else (b, a)
                | a <- Set.toList def
                , b <- Set.toList liveOut
                , a /= b
                ]
