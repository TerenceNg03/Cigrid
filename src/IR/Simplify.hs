{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module IR.Simplify (simplify, mendCallRets, safeRegisters, calleeSaved, callerSaved) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import IR.Types (IR (..), IRBlock (..), IRFunc (..), IRInst (Add, IRComment, Pop, Push, Sub), IRJump (..), IRReg (..), RegImm (Constant))
import Optics ((%~), (&), (^.))

-- | Remove randuant jumps and unreachable code
simplify :: IR -> IR
simplify = removeUnreachable . (#funcs %~ Map.map squashLinearBlocks)

-- | Rbx, R10, R11 are used during spilling
safeRegisters :: Set IRReg
safeRegisters = [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, Rbp, R12, R13, R14, R15]

calleeSaved :: Set IRReg
calleeSaved = [Rbp, R12, R13, R14, R15]

callerSaved :: Set IRReg
callerSaved = [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9]

-- | Mend function calls and returns
mendCallRets :: IR -> IR
mendCallRets = #funcs %~ Map.map (mendCall . mendRet)

mendCall :: IRFunc -> IRFunc
mendCall f@IRFunc{..} =
    let saveRegs = Set.intersection calleeSaved usedReg
        saveRegsInsts = Push <$> Set.toAscList saveRegs
        reserveStack
            | stackSize > 0 = [Sub Rsp $ Constant stackSize]
            | otherwise = []
        blocks' =
            Map.update
                ( Just
                    . ( \b@IRBlock{..} ->
                            b
                                { insts = saveRegsInsts ++ reserveStack ++ insts
                                , label2 = Just name
                                }
                      )
                )
                root
                blocks
     in f{blocks = blocks'}

mendRet :: IRFunc -> IRFunc
mendRet f@IRFunc{blocks, stackSize, usedReg} = f{blocks = process <$> blocks}
  where
    process block@IRBlock{flow} =
        case flow of
            Jmp _ -> block
            Branch{} -> block
            Ret
                | stackSize > 0 -> block & #insts %~ (++ Add Rsp (Constant stackSize) : restoreRegsInsts)
                | otherwise -> block & #insts %~ (++ restoreRegsInsts)
      where
        restoreRegs = Set.intersection calleeSaved usedReg
        restoreRegsInsts = Pop <$> Set.toDescList restoreRegs

isEmpty :: [IRInst] -> Bool
isEmpty [] = True
isEmpty (IRComment{} : xs) = isEmpty xs
isEmpty _ = False

findReachable :: Set Int -> Map Int IRBlock -> Int -> [IRBlock]
findReachable s m root =
    if Set.member root s
        then []
        else
            let s' = Set.insert root s
             in case m ! root ^. #flow of
                    Jmp b -> (m ! root) : findReachable s' m b
                    Branch _ b1 b2 -> (m ! root) : findReachable s' m b1 ++ findReachable s' m b2
                    Ret -> [m ! root]

removeUnreachable :: IR -> IR
removeUnreachable = #funcs %~ Map.map process
  where
    process :: IRFunc -> IRFunc
    process func@IRFunc{blocks, root} =
        let blocks' = (\b@IRBlock{label} -> (label, b)) <$> findReachable mempty blocks root
         in func{blocks = Map.fromList blocks'}

converge :: (Eq a) => (a -> a) -> a -> a
converge f x
    | f x == x = x
    | otherwise = converge f (f x)

squashLinearBlocks :: IRFunc -> IRFunc
squashLinearBlocks f@IRFunc{blocks} =
    f
        { blocks =
            Map.fromList ((\b -> (b ^. #label, b)) <$> converge processBlocks (Map.elems blocks))
        }
  where
    inc = Map.alter (Just . maybe 1 (+ 1))
    countInDegrees :: [IRBlock] -> Map Int Int -> Map Int Int
    countInDegrees (IRBlock{flow} : xs) m =
        case flow of
            Jmp i -> countInDegrees xs $ inc i m
            Branch _ i1 i2 -> countInDegrees xs $ inc i1 $ inc i2 m
            Ret -> countInDegrees xs m
    countInDegrees [] m = m
    inDegrees = countInDegrees (Map.elems blocks) mempty
    processBlocks :: [IRBlock] -> [IRBlock]
    processBlocks (b@(IRBlock _ _ insts (Jmp i)) : xs)
        | inDegrees ! i == 1 || isEmpty insts =
            (b{insts = insts ++ blocks ! i ^. #insts, flow = blocks ! i ^. #flow}) : processBlocks xs
    processBlocks (b@(IRBlock _ _ insts (Branch (Constant i) bt bf)) : xs)
        | i == 0 =
            (b{insts = insts ++ blocks ! bf ^. #insts, flow = blocks ! bf ^. #flow}) : processBlocks xs
        | otherwise =
            (b{insts = insts ++ blocks ! bt ^. #insts, flow = blocks ! bt ^. #flow}) : processBlocks xs
    processBlocks (x : xs) = x : processBlocks xs
    processBlocks [] = []
