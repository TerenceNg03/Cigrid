{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lex (identL, opL, numL, LexM, stringL, charL, commentL, lexL, runLex) where

import Ast.Ast (Ast, Phase (Lexed))
import Ast.AstSrc (AstSrc (AstSrc))
import Ast.Token (Token (..))
import Data.Char (isAlpha, isSpace, ord)
import Data.List (group, isPrefixOf, sort, sortBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Diagnose (reportErr)
import Error.Diagnose (Position (Position), Report)
import Fmt ((+|), (|+))
import Source (Loc, Source (filename, loc, rest), nextN)
import Text.RE ()
import Text.RE.TDFA (RE, compileRegex, matchedText, (?=~))

type instance Ast Lexed a = AstSrc a

newtype LexM a = LexM {runLexM :: Source -> [Ast Lexed Token] -> Either (Position, Report Text) (a, Source, [Ast Lexed Token])}

instance Functor LexM where
    fmap :: (a -> b) -> LexM a -> LexM b
    fmap f m = LexM $ \s l -> (\(a, s', l') -> (f a, s', l')) <$> runLexM m s l

instance Applicative LexM where
    pure :: a -> LexM a
    pure x = LexM $ \s l -> Right (x, s, l)
    (<*>) :: LexM (a -> b) -> LexM a -> LexM b
    m1 <*> m2 = do
        f <- m1
        f <$> m2

instance Monad LexM where
    (>>=) :: LexM a -> (a -> LexM b) -> LexM b
    m >>= f = LexM $ \s l -> do
        (a, src, tl) <- runLexM m s l
        runLexM (f a) src tl

instance MonadFail LexM where
    fail :: String -> LexM a
    fail s = do
        l <- here
        throw l l "Internal failure" (pack s)

-- | Look Ahead
peek :: LexM String
peek = LexM $ \s l -> Right (s.rest, s, l)

-- | Current source location
here :: LexM Loc
here = LexM $ \s l -> Right (s.loc, s, l)

-- | Skip n character in source
skip :: Int -> LexM ()
skip n = LexM $ \s l -> Right ((), nextN n s, l)

-- | Append a parsed token
put :: Loc -> Loc -> Token -> LexM ()
put start end t = LexM $ \s l -> Right ((), s, AstSrc (Position start end s.filename) t : l)

-- | Throw an error with location and description
throw :: Loc -> Loc -> Text -> Text -> LexM a
throw start end msg hint = LexM $ \s _ ->
    let posi = Position start end s.filename
     in Left (posi, reportErr posi msg hint)

regexL :: RE -> LexM String
regexL re = do
    top <- peek
    case matchedText $ top ?=~ re of
        Just s -> do
            skip $ length s
            return s
        Nothing -> do
            l <- here
            throw l l "Internal error: RegexL find no matches" ""

identL :: LexM ()
identL = do
    start <- here
    re <- compileRegex "^[_a-zA-Z][_a-zA-Z0-9]*"
    s <- regexL re
    end <- here
    put start end $ case s of
        "break" -> Break
        "extern" -> Extern
        "new" -> New
        "while" -> While
        "char" -> CharT
        "for" -> For
        "return" -> Return
        "delete" -> Delete
        "if" -> If
        "struct" -> Struct
        "else" -> Else
        "int" -> IntT
        "void" -> Void
        _ -> Identifier $ pack s

isOp :: Char -> Bool
isOp c = c `elem` l
  where
    l = map head . group . sort . concat $ Map.keys ops

ops :: Map String Token
ops =
    [ (",", Comma)
    , (".", Period)
    , (";", Semicolon)
    , ("=", Assign)
    , ("+", Plus)
    , ("-", Minus)
    , ("*", Mul)
    , ("/", Div)
    , ("%", Mod)
    , ("<", Lt)
    , ("<=", Le)
    , (">=", Ge)
    , (">", Gt)
    , ("==", Eq)
    , ("!=", Ne)
    , ("&&", And)
    , ("!", Not)
    , ("||", Or)
    , ("~", BitNot)
    , ("&", BitAnd)
    , ("|", BitOr)
    , ("<<", LeftShift)
    , (">>", RightShift)
    , ("[", LBracket)
    , ("]", RBracket)
    , ("(", LParenthesis)
    , (")", RParenthesis)
    , ("{", LBrace)
    , ("}", RBrace)
    , ("++", Increment)
    , ("--", Decrement)
    ]

opL :: LexM ()
opL = do
    start <- here
    s <- peek
    let matched = filter (`isPrefixOf` s) (Map.keys ops)
        longest = take 1 $ sortBy (flip (\x y -> compare (length x) (length y))) matched
    case longest of
        [op] -> do
            skip $ length op
            end <- here
            put start end $ ops ! op
        [] -> do
            op' <- scanOp
            skip $ length op'
            end <- here
            throw start end ("Invalid operator: " +| op' |+ "") "here"
        _ -> error "unreachable"
  where
    scanOp = do
        s' <- peek
        case s' of
            (x : _) | isOp x -> do
                skip 1
                left <- scanOp
                return (x : left)
            _ -> do
                return []

numL :: LexM ()
numL = do
    start <- here
    re <- compileRegex "^(0|[1-9][0-9]*|0[xX][0-9a-fA-F]+)"
    num <- regexL re
    end <- here
    put start end $ Integer $ read num

{- | Handle escape sequence
|
| Left "Invalid escape sequence" Right "Unescaped string"
-}
unescape :: String -> Either String String
unescape [] = Right []
unescape ('\\' : 'n' : xs) = ('\n' :) <$> unescape xs
unescape ('\\' : 't' : xs) = ('\t' :) <$> unescape xs
unescape ('\\' : '\\' : xs) = ('\\' :) <$> unescape xs
unescape ('\\' : '\'' : xs) = ('\'' :) <$> unescape xs
unescape ('\\' : '\"' : xs) = ('\"' :) <$> unescape xs
unescape ('\\' : x : _) = Left $ "\\" +| x |+ ""
unescape (x : xs)
    | x == '"' || x == '\'' || x == '\n' || x == '\t' = Left $ "" +| show x |+ " is not escaped"
    | otherwise = (x :) <$> unescape xs

stringL :: LexM ()
stringL = do
    start <- here
    skip 1
    s <- scanString start
    end <- here
    case unescape s of
        Left err -> throw start end ("Invalid escape sequence: " +| err |+ "") "While lexing this"
        Right s' -> put start end $ String $ pack s'
  where
    scanString start = do
        s <- peek
        case s of
            ('\\' : x : _) -> do
                skip 2
                left <- scanString start
                return $ '\\' : x : left
            ('\"' : _) -> skip 1 >> return []
            (x : _) -> skip 1 >> (x :) <$> scanString start
            [] -> do
                end <- here
                throw start end "Incomplete string literal" "While lexing this"

charL :: LexM ()
charL = do
    start <- here
    skip 1
    s <- scanChar start
    end <- here
    case unescape s of
        Left err -> throw start end ("Invalid escape sequence: " +| err |+ "") "While lexing this"
        Right [x] -> put start end $ Char x
        Right _ -> throw start end "Not exactly one character: " "While lexing this"
  where
    scanChar start = do
        s <- peek
        case s of
            ('\\' : x : _) -> do
                skip 2
                left <- scanChar start
                return $ '\\' : x : left
            ('\'' : _) -> skip 1 >> return []
            (x : _) -> skip 1 >> (x :) <$> scanChar start
            [] -> do
                end <- here
                throw start end "Incomplete string literal" "Start from here"

whiteSpaceL :: LexM ()
whiteSpaceL = do
    s <- peek
    case s of
        (x : _) | isSpace x -> skip 1 >> whiteSpaceL
        _ -> return ()

skipLine :: LexM ()
skipLine = do
    s <- peek
    case s of
        ('\n' : _) -> skip 1
        (_ : _) -> skip 1 >> skipLine
        [] -> return ()

commentL :: LexM ()
commentL = do
    s <- peek
    case s of
        ('/' : '/' : _) -> skipLine
        ('/' : '*' : _) -> do
            skipMultilineComment =<< here
        _ -> error "Unreachable"
  where
    skipMultilineComment start = do
        s <- peek
        case s of
            ('*' : '/' : _) -> skip 2
            (_ : _) -> skip 1 >> skipMultilineComment start
            [] -> do
                end <- here
                throw start end "Multiline comment is not completed" "Start from here"

macroL :: LexM ()
macroL = skipLine

lexL :: LexM ()
lexL = do
    s <- peek
    if s == ""
        then return ()
        else do
            case s of
                ('/' : '*' : _) -> commentL
                ('/' : '/' : _) -> commentL
                ('"' : _) -> stringL
                ('\'' : _) -> charL
                ('#' : _) -> macroL
                (x : _)
                    | isSpace x -> whiteSpaceL
                    | isOp x -> opL
                    | isAlpha x || x == '_' -> identL
                    | ord x <= ord '9' && ord x >= ord '0' -> numL
                    | otherwise -> do
                        start <- here
                        skip 1
                        end <- here
                        throw start end ("Invalid character: " +| x |+ "") "here"
                [] -> error "Unreachable"
            lexL

runLex :: LexM a -> Source -> Either (Position, Report Text) [Ast Lexed Token]
runLex m src = (\(_, _, l) -> reverse l) <$> runLexM m src []
