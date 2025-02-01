{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module IR.Utils (newReg, unsupported, newBlock, callRegMap, newReg', register) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (gets)
import qualified Data.Map as Map
import Data.Text (Text)
import Error.Diagnose (Marker (This), Position, Report (Err))
import IR.Types (FuncState, GenM, IRBlock, IRReg (..), IRState)
import Optics ((%), (^.), _1, _2)
import Optics.State.Operators ((%=), (.=))
import Source (HasPosition (pos))

newReg :: Text -> GenM (IRState, FuncState) IRReg
newReg name = do
    regMap <- gets (^. _2 % #regMap)
    let reg = Map.size regMap
    _2 % #regMap %= Map.insert reg (Just name)
    return $ SpillReg reg

newReg' :: GenM (IRState, FuncState) IRReg
newReg' = do
    regMap <- gets (^. _2 % #regMap)
    let reg = Map.size regMap
    _2 % #regMap %= Map.insert reg Nothing
    return $ SpillReg reg

newBlock :: GenM (IRState, FuncState) Int
newBlock = do
    block <- gets (^. _1 % #nextBlockLabel)
    _1 % #nextBlockLabel .= block + 1
    return block

register :: IRBlock -> GenM (IRState, FuncState) ()
register b = do
    _2 % #blocks %= Map.insert (b ^. #label) b

unsupported :: (HasPosition a, MonadError (Position, Report Text) m) => Text -> a -> m b
unsupported msg p =
    throwError $
        (pos p,) $
            Err Nothing "Unsupported operation." [(pos p, This msg)] []

callRegMap :: [IRReg]
callRegMap =
    [ Rdi
    , Rsi
    , Rdx
    , Rcx
    , R8
    , R9
    ]
