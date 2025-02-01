{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.Stmt (genStmt) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Typed))
import Ast.Stmt (Stmt (..))
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import IR.Expr (genExpr)
import IR.Types (FallBack (..), FuncState, GenM, IRBlock (IRBlock), IRInst (..), IRJump (Branch, Jmp, Ret), IRReg (Rax, Rdi), IRState, RegImm (TempReg))
import IR.Utils (newBlock, newReg, register, unsupported)
import Optics ((%), (%~), (^.), _1, _2)
import Optics.State.Operators ((%=), (.=))
import Source (pos)
import TypeCheck.Types (AstTyped (AstTyped), CiType (Char, Ptr, Struct), HasType (getType))

genStmt :: [Ast Typed (Stmt Typed)] -> GenM (IRState, FuncState) IRBlock
genStmt [] = do
    fallback <- gets (^. _2 % #fallBack)
    blk <- newBlock
    case fallback of
        Goto target -> return $ IRBlock blk Nothing [] (Jmp target)
        RetVoid -> return $ IRBlock blk Nothing [] Ret
genStmt (AstTyped p _ stmt : xs) = do
    let build is = (#insts %~ (is ++)) <$> genStmt xs
    case stmt of
        SExpr expr -> do
            src <- asks (^. #source)
            is <- snd <$> genExpr expr
            build (IRComment p src : is)
        SVarDef _ name expr -> do
            src <- asks (^. #source)
            reg <- newReg (underlay name)
            _1 % #nameMap %= Map.insert (underlay name) (Left reg)
            (reg', is) <- genExpr expr
            build (IRComment p src : is ++ [Mov reg reg'])
        SVarAssign name expr -> do
            src <- asks (^. #source)
            m <- gets (^. _1 % #nameMap)
            (reg', is) <- genExpr expr
            case m ! underlay name of
                Left reg -> build (IRComment p src : is ++ [Mov reg reg'])
                Right label -> build (IRComment p src : is ++ [Sa label reg'])
        SArrayAssign name idx field value -> do
            src <- asks (^. #source)
            when (isJust field) $
                unsupported "Access struct field" p
            (regIx, isIx) <- genExpr idx
            (regVal, isVal) <- genExpr value
            m <- gets (^. _1 % #nameMap)
            let regS = m ! underlay name
            op <- case getType name of
                Ptr Char -> return Sb
                Ptr (Struct _) -> unsupported "Struct is not supported" p
                Ptr _ -> return Sqw
                _ -> error "Unreachable"
            case regS of
                Left reg -> build (IRComment p src : isIx ++ isVal ++ [op regVal reg regIx])
                Right label -> build (IRComment p src : isIx ++ isVal ++ [op regVal Rax regIx, Sa label $ TempReg Rax])
        SScope stmts -> do
            next <- genStmt xs
            register next
            fallbackOld <- gets (^. _2 % #fallBack)
            posOld <- gets (^. _2 % #blockPosition)
            _2 % #fallBack .= Goto (next ^. #label)
            _2 % #blockPosition .= p
            b <- genStmt stmts
            _2 % #fallBack .= fallbackOld
            _2 % #blockPosition .= posOld
            return b
        SIf cond body elseBody -> do
            src <- asks (^. #source)
            (reg, is') <- genExpr cond
            let is = IRComment p src : is'
            next <- genStmt xs
            register next
            fallbackOld <- gets (^. _2 % #fallBack)
            posOld <- gets (^. _2 % #blockPosition)
            _2 % #fallBack .= Goto (next ^. #label)
            _2 % #blockPosition .= pos body
            bt <- genStmt [body]
            register bt
            bfm <- case elseBody of
                Just eb -> do
                    _2 % #blockPosition .= pos eb
                    ebb <- genStmt [eb]
                    return $ Just ebb
                Nothing -> return Nothing
            _2 % #fallBack .= fallbackOld
            _2 % #blockPosition .= posOld
            label <- newBlock
            case bfm of
                Just bf -> do
                    register bf
                    return $ IRBlock label Nothing is (Branch reg (bt ^. #label) (bf ^. #label))
                Nothing -> return $ IRBlock label Nothing is (Branch reg (bt ^. #label) (next ^. #label))
        SWhile cond body -> do
            src <- asks (^. #source)
            label <- newBlock
            (reg, is') <- genExpr cond
            let is = IRComment p src : is'
            next <- genStmt xs
            register next
            fallbackOld <- gets (^. _2 % #fallBack)
            posOld <- gets (^. _2 % #blockPosition)
            _2 % #fallBack .= Goto label
            _2 % #blockPosition .= pos body
            bt <- genStmt [body]
            register bt
            _2 % #fallBack .= fallbackOld
            _2 % #blockPosition .= posOld
            register $ IRBlock label Nothing is (Branch reg (bt ^. #label) (next ^. #label))
            label' <- newBlock
            return $ IRBlock label' Nothing [] (Jmp label)
        SReturn expr -> do
            label <- newBlock
            src <- asks (^. #source)
            case expr of
                Just expr' -> do
                    (reg, is) <- genExpr expr'
                    return $ IRBlock label Nothing (IRComment p src : is ++ [Mov Rax reg]) Ret
                Nothing -> return $ IRBlock label Nothing [IRComment p src] Ret
        SDelete name -> do
            src <- asks (^. #source)
            m <- gets (^. (_1 % #nameMap))
            case m ! underlay name of
                Left reg -> build [IRComment p src, Mov Rdi $ TempReg reg, Call "free"]
                Right label -> build [IRComment p src, La Rdi label, Call "free"]
        -- Break Just goes to the fallback indirection of current block
        SBreak -> do
            src <- asks (^. #source)
            (#insts %~ (IRComment p src :)) <$> genStmt []
