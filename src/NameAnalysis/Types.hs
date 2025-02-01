{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NameAnalysis.Types (NameM, Scope (..), NameState (..), runNameAnalysis) where

import Ast.Ast (Ast, Phase (Named))
import Ast.AstSrc (AstSrc)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Error.Diagnose (Position, Report)
import Parse.Types ()

type instance Ast Named a = AstSrc a

data NameState = Declared | Defined
data Scope = Scope
    { genId :: Int
    , sname :: [Text]
    , resolved :: Map Text (NameState, Position)
    }

type NameM a = StateT Scope (Except (Position, Report Text)) a

runNameAnalysis :: (b -> NameM a) -> b -> Either (Position, Report Text) a
runNameAnalysis m b =
    let scope = Scope 0 [] Map.empty
     in runExcept $ evalStateT (m b) scope
