module Ast.Token (Token (..)) where

import Data.Text (Text)

data Token
    = Identifier Text
    | Integer Int
    | String Text
    | Char Char
    | Break
    | Extern
    | New
    | While
    | CharT
    | For
    | Return
    | Delete
    | If
    | Struct
    | Else
    | IntT
    | Void
    | Plus
    | Minus
    | Mul
    | Div
    | Mod
    | Lt
    | Le
    | Ge
    | Gt
    | Eq
    | Ne
    | And
    | Not
    | Or
    | BitNot
    | BitAnd
    | BitOr
    | LeftShift
    | RightShift
    | LBracket
    | RBracket
    | LBrace
    | RBrace
    | LParenthesis
    | RParenthesis
    | Comma
    | Assign
    | Semicolon
    | Period
    | Increment
    | Decrement
    deriving (Eq, Show)
