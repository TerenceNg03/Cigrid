module Ast.ShowText (showText, showChar) where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty))
import Prelude hiding (showChar)

showText :: Text -> Doc ann
showText = pretty . process . show

process :: [Char] -> [Char]
process ('\'' : xs) = '\\' : '\'' : process xs
process (x : xs) = x : process xs
process [] = []

showChar :: Char -> Doc ann
showChar c = case c of
    '\"' -> pretty "'\\\"'"
    c' -> pretty $ show c'
