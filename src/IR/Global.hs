{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.Global (genGlb, genGlbs) where

import Ast.Ast (Ast, IsAst (underlay), Phase (Typed))
import Ast.Expr (Expr (EInt))
import Ast.Global (Global (..))
import Control.Monad (forM, forM_)
import Control.Monad.State (MonadTrans (lift), execStateT, get, gets, put)
import qualified Data.Map as Map
import qualified Data.Set as Set
import IR.Simplify (simplify)
import IR.Stmt (genStmt)
import IR.Types (FallBack (..), FuncState (..), GenM, IRFunc (..), IRInst (Mov), IRM, IRState, RegImm (VarReg))
import IR.Utils (callRegMap, newReg, register, unsupported)
import Optics ((%), (%~), (&), (^.), _1, _2)
import Optics.State.Operators ((%=), (.=))
import Source (pos)
import TypeCheck.Types (AstTyped (AstTyped))

withFunc :: FuncState -> GenM (IRState, FuncState) () -> GenM IRState ()
withFunc fs m = do
    irs <- get
    (irs', _) <- lift $ execStateT m (irs, fs)
    put irs'

genGlb :: Ast Typed (Global Typed) -> IRM ()
genGlb (AstTyped p _ glb) =
    case glb of
        GFuncDef _ fname params' body ->
            let fs =
                    FuncState
                        { regMap = mempty
                        , fallBack = RetVoid
                        , blockPosition = p
                        , blocks = mempty
                        }
             in withFunc fs $ do
                    pRegs <- forM params' $ \(_, pname) -> do
                        reg <- newReg $ underlay pname
                        _1 % #nameMap %= Map.insert (underlay pname) (Left reg)
                        return reg
                    _2 % #blockPosition .= pos body
                    b' <- genStmt [body]
                    let callInsts = uncurry Mov <$> zip pRegs (VarReg <$> callRegMap)
                        b = b' & #insts %~ (callInsts ++)
                    register b
                    m <- gets (^. (_2 % #regMap))
                    blks <- gets (^. (_2 % #blocks))

                    let func =
                            IRFunc
                                { name = underlay fname
                                , params = pRegs
                                , regMap = m
                                , root = b ^. #label
                                , blocks = blks
                                , stackSize = Map.size m * 8
                                , usedReg = Set.empty
                                }
                    _1 % #ir % #funcs %= Map.insert (underlay fname) func
                    return ()
        GFuncDecl _ fname _ ->
            #ir % #externs %= Set.insert (underlay fname)
        GVarDecl _ vname ->
            #ir % #externs %= Set.insert (underlay vname)
        GStruct _ _ -> return ()
        GVarDef _ vname (AstTyped _ _ (EInt i)) -> do
            #ir % #globalVars %= Map.insert (underlay vname) i
            #nameMap %= Map.insert (underlay vname) (Right $ underlay vname)
        GVarDef{} -> unsupported "Global variables must be initialized with an integerx." p

genGlbs :: [Ast Typed (Global Typed)] -> IRM ()
genGlbs glbs = forM_ glbs genGlb >> #ir %= simplify
