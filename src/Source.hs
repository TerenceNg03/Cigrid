{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Source (Loc, Source (..), next, newSource, nextN, newSource', (<->), HasPosition (pos)) where

import Error.Diagnose (Position (Position))

type Loc = (Int, Int)

class HasPosition a where
    pos :: a -> Position

instance HasPosition Position where
    pos :: Position -> Position
    pos = id

(<->) :: (HasPosition a, HasPosition b) => a -> b -> Position
a <-> b =
    let (Position x _ f) = pos a
        (Position _ y _) = pos b
     in Position x y f

data Source = Source
    { loc :: Loc
    , filename :: String
    , rest :: String
    }
    deriving (Eq)

newSource :: String -> String -> Source
newSource = Source (1, 1)

newSource' :: String -> Source
newSource' = Source (1, 1) ""

next :: Source -> Source
next (Source loc' fn []) = Source loc' fn []
next (Source (ln', _) fn ('\n' : xs)) = Source (ln' + 1, 1) fn xs
next (Source (ln', col') fn (_ : xs)) = Source (ln', col' + 1) fn xs

nextN :: Int -> Source -> Source
nextN n s = iterate next s !! n
