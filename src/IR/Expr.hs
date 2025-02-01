{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.Expr (genExpr) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Typed))
import qualified Ast.BOp as BOp
import Ast.Expr (Expr (..))
import qualified Ast.UOp as UOp
import Control.Monad (forM, when)
import Control.Monad.State (gets)
import Data.Bits (Bits (complement, shift, (.|.)), (.&.))
import Data.Char (ord)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Fmt ((+|), (|+))
import IR.Types (FuncState, GenM, IRInst (..), IRReg (..), IRState, RegImm (Constant, TempReg, VarReg))
import IR.Utils (callRegMap, newReg, newReg', unsupported)
import Optics ((%), (^.), _1)
import Optics.State.Operators ((%=))
import TypeCheck.Types (AstTyped (AstTyped), CiType (Char, Ptr, Struct), HasType (getType))

genExpr :: Ast Typed (Expr Typed) -> GenM (IRState, FuncState) (RegImm, [IRInst])
genExpr (AstTyped p _ expr) =
    case expr of
        EVar name -> do
            m <- gets (^. _1 % #nameMap)
            case m ! name of
                Left reg -> return (VarReg reg, [])
                Right label -> do
                    reg <- newReg name
                    return (TempReg reg, [Lsv reg label])
        EInt i -> do
            return (Constant i, [])
        EChar c -> do
            return (Constant $ ord c, [])
        EString t -> do
            label <- gets (^. _1 % #nextStrLabel)
            _1 % #nextStrLabel %= (+ 1)
            _1 % #ir % #strings %= Map.insert label t
            reg <- newReg'
            return (TempReg reg, [La reg ("str_" +| label |+ "")])
        ECall name exprs -> do
            exprs' <- forM exprs genExpr
            when (length exprs > 6) $
                unsupported "Too many parameters" p
            let regs = fst <$> exprs'
                is = concatMap snd exprs'
                prepare = zipWith Mov callRegMap regs
            reg <- newReg'
            return (TempReg reg, is ++ prepare ++ [Call (underlay name), Mov reg $ TempReg Rax])
        ENew _ expr' -> do
            (reg, is) <- genExpr expr'
            case getType expr' of
                Struct _ -> unsupported "Alloc struct array" p
                Char -> do
                    reg' <- newReg'
                    return (TempReg reg', is ++ [Mov Rdi reg, Call "malloc", Mov reg' $ TempReg Rax])
                _ -> do
                    reg' <- newReg'
                    return
                        ( TempReg reg'
                        , is
                            ++ [ Mov Rdi reg
                               , Mov Rcx 3
                               , Shl Rdi
                               , Call "malloc"
                               , Mov reg' $ TempReg Rax
                               ]
                        )
        EArrayAccess name idx field -> do
            when (isJust field) $
                unsupported "Access struct field" p
            (regIx, isIx) <- genExpr idx
            m <- gets (^. _1 % #nameMap)
            (regS, isS) <- case m ! underlay name of
                Left r -> do
                    reg <- newReg'
                    return (reg, [Mov reg $ TempReg r])
                Right label -> do
                    reg <- newReg'
                    return (reg, [Lsv reg label])
            case getType name of
                Ptr Char -> return (TempReg regS, isIx ++ isS ++ [Lb regS regIx])
                Ptr (Struct _) -> unsupported "Struct is not supported" p
                Ptr _ -> return (TempReg regS, isIx ++ isS ++ [Lqw regS regIx])
                _ -> error "Unreachable"
        EBinOp (AstTyped _ _ BOp.Plus) lhs rhs -> genNormalBinOp lhs rhs Add (+)
        EBinOp (AstTyped _ _ BOp.MinusB) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ i1 - i2, linsts ++ rinsts)
                (TempReg r, _) -> return (lreg, linsts ++ rinsts ++ [Sub r rreg])
                _ -> do
                    reg <- newReg'
                    return (TempReg reg, linsts ++ rinsts ++ [Mov reg lreg, Sub reg rreg])
        EBinOp (AstTyped _ _ BOp.Mul) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ i1 * i2, linsts ++ rinsts)
                _ -> do
                    reg <- getTemp lreg rreg
                    return
                        ( TempReg reg
                        , linsts
                            ++ rinsts
                            ++ [ Mov Rax lreg
                               , Mov Rdx rreg
                               , Mul Rdx
                               , Mov reg $ TempReg Rax
                               ]
                        )
        EBinOp (AstTyped _ _ BOp.Div) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ i1 `div` i2, linsts ++ rinsts)
                _ -> do
                    reg <- getTemp lreg rreg
                    return
                        ( TempReg reg
                        , linsts
                            ++ rinsts
                            ++ [ Mov Rax lreg
                               , Cqo
                               , Mov reg rreg
                               , Div reg
                               , Mov reg $ TempReg Rax
                               ]
                        )
        EBinOp (AstTyped _ _ BOp.Mod) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ i1 `mod` i2, linsts ++ rinsts)
                _ -> do
                    reg <- getTemp lreg rreg
                    return
                        ( TempReg reg
                        , linsts
                            ++ rinsts
                            ++ [ Mov Rax lreg
                               , Cqo
                               , Mov reg rreg
                               , Div reg
                               , Mov reg $ TempReg Rdx
                               ]
                        )
        EBinOp (AstTyped _ _ BOp.Ge) lhs rhs -> genCmp lhs rhs SetGe SetL (>=)
        EBinOp (AstTyped _ _ BOp.Lt) lhs rhs -> genCmp lhs rhs SetL SetGe (<)
        EBinOp (AstTyped _ _ BOp.Le) lhs rhs -> genCmp lhs rhs SetLe SetG (<=)
        EBinOp (AstTyped _ _ BOp.Gt) lhs rhs -> genCmp lhs rhs SetG SetLe (>)
        EBinOp (AstTyped _ _ BOp.Eq) lhs rhs -> genCmp lhs rhs SetE SetNe (==)
        EBinOp (AstTyped _ _ BOp.Ne) lhs rhs -> genCmp lhs rhs SetNe SetE (/=)
        EBinOp (AstTyped _ _ BOp.BitAnd) lhs rhs -> genNormalBinOp lhs rhs And (.&.)
        EBinOp (AstTyped _ _ BOp.BitOr) lhs rhs -> genNormalBinOp lhs rhs Or (.|.)
        EBinOp (AstTyped _ _ BOp.LeftShift) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ i1 `shift` i2, linsts ++ rinsts)
                (TempReg r, _) -> return (lreg, linsts ++ rinsts ++ [Mov Rcx rreg, Shl r])
                _ -> do
                    reg <- getTemp lreg rreg
                    return (TempReg reg, linsts ++ rinsts ++ [Mov Rcx rreg, Mov reg lreg, Shl reg])
        EBinOp (AstTyped _ _ BOp.RightShift) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ i1 `shift` (-i2), linsts ++ rinsts)
                (TempReg r, _) -> return (lreg, linsts ++ rinsts ++ [Mov Rcx rreg, Shr r])
                _ -> do
                    reg <- getTemp lreg rreg
                    return (TempReg reg, linsts ++ rinsts ++ [Mov Rcx rreg, Mov reg lreg, Shr reg])
        EBinOp (AstTyped _ _ BOp.And) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ if (i1 /= 0) && (i2 /= 0) then 1 else 0, linsts ++ rinsts)
                (_, _) -> do
                    reg1 <- newReg'
                    reg2 <- newReg'
                    return (TempReg reg1, linsts ++ rinsts ++ [Mov reg1 lreg, Mov reg2 rreg, Cmp reg2 0, CMovNZ reg1 reg2, Cmp reg1 0, SetNZ reg1])
        EBinOp (AstTyped _ _ BOp.Or) lhs rhs -> do
            (lreg, linsts) <- genExpr lhs
            (rreg, rinsts) <- genExpr rhs
            case (lreg, rreg) of
                (Constant i1, Constant i2) -> return (Constant $ if (i1 /= 0) || (i2 /= 0) then 1 else 0, linsts ++ rinsts)
                (_, _) -> do
                    reg1 <- newReg'
                    reg2 <- newReg'
                    return (TempReg reg1, linsts ++ rinsts ++ [Mov reg1 lreg, Mov reg2 rreg, Cmp reg2 0, CMovZ reg1 reg2, Cmp reg1 0, SetNZ reg1])
        EUnOp (AstTyped _ _ UOp.MinusU) expr' -> do
            (reg, is) <- genExpr expr'
            case reg of
                Constant i -> return (Constant (-i), is)
                TempReg r -> return (reg, is ++ [Neg r])
                VarReg _ -> do
                    reg' <- newReg'
                    return (TempReg reg', is ++ [Mov reg' reg, Neg reg'])
        EUnOp (AstTyped _ _ UOp.BitNot) expr' -> do
            (reg, is) <- genExpr expr'
            case reg of
                Constant i -> return (Constant (complement i), is)
                TempReg r -> return (reg, is ++ [Not r])
                VarReg _ -> do
                    reg' <- newReg'
                    return (TempReg reg', is ++ [Mov reg' reg, Not reg'])
        EUnOp (AstTyped _ _ UOp.Not) expr' -> do
            (reg, is) <- genExpr expr'
            case reg of
                Constant i -> return (if i == 0 then 1 else 0, is)
                TempReg r -> return (reg, is ++ [Cmp r 0, SetZ r])
                VarReg r -> do
                    r' <- newReg'
                    return (TempReg r', is ++ [Cmp r 0, SetZ r'])

genCmp :: AstTyped (Expr Typed) -> AstTyped (Expr Typed) -> (IRReg -> IRInst) -> (IRReg -> IRInst) -> (Int -> Int -> Bool) -> GenM (IRState, FuncState) (RegImm, [IRInst])
genCmp lhs rhs op opNot opConst = do
    (lreg, linsts) <- genExpr lhs
    (rreg, rinsts) <- genExpr rhs
    case (lreg, rreg) of
        (Constant i1, Constant i2) -> return (if i1 `opConst` i2 then 1 else 0, linsts ++ rinsts)
        (TempReg r, _) -> return (lreg, linsts ++ rinsts ++ [Cmp r rreg, op r])
        (_, TempReg r) -> return (rreg, linsts ++ rinsts ++ [Cmp r lreg, opNot r])
        _ -> do
            reg <- newReg'
            return (TempReg reg, linsts ++ rinsts ++ [Mov reg lreg, Cmp reg rreg, op reg])

genNormalBinOp :: AstTyped (Expr Typed) -> AstTyped (Expr Typed) -> (IRReg -> RegImm -> IRInst) -> (Int -> Int -> Int) -> GenM (IRState, FuncState) (RegImm, [IRInst])
genNormalBinOp lhs rhs op opConst = do
    (lreg, linsts) <- genExpr lhs
    (rreg, rinsts) <- genExpr rhs
    case (lreg, rreg) of
        (Constant i1, Constant i2) -> return (Constant $ i1 `opConst` i2, linsts ++ rinsts)
        (TempReg r, _) -> return (lreg, linsts ++ rinsts ++ [op r rreg])
        (_, TempReg r) -> return (rreg, linsts ++ rinsts ++ [op r lreg])
        _ -> do
            reg <- newReg'
            return (TempReg reg, linsts ++ rinsts ++ [Mov reg lreg, op reg rreg])

getTemp :: RegImm -> RegImm -> GenM (IRState, FuncState) IRReg
getTemp (TempReg r) _ = return r
getTemp _ (TempReg r) = return r
getTemp _ _ = newReg'
