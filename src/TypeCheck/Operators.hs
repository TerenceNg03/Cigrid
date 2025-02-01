module TypeCheck.Operators (unifyBOp, unifyUOp) where

import Ast.BOp (BOp (..))
import Ast.UOp (UOp)
import Error.Diagnose (Position)
import TypeCheck.Types (CiType (..), TypeM)
import TypeCheck.Utils (unify)

numBOp :: Position -> CiType -> CiType -> TypeM CiType
numBOp sp t1 t2 = do
    unify sp t1 (Int False)
    unify sp t2 (Int False)
    return $ Int False

unifyBOp :: BOp -> Position -> CiType -> CiType -> TypeM CiType
unifyBOp op
    | op == Eq || op == Ne = \sp t1 t2 -> do
        unify sp t1 t2
        return $ Int False
    | otherwise = numBOp

unifyUOp :: UOp -> Position -> CiType -> TypeM CiType
unifyUOp _ sp ty = do
    unify sp ty (Int False)
    return $ Int False
