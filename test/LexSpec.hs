{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LexSpec (spec) where

import Ast.Ast (IsAst (underlay))
import Ast.Token (Token (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isRight)
import Data.Text (Text, pack)
import Error.Diagnose (Position, Report, TabSize (TabSize), WithUnicode (WithUnicode), addFile, addReport, prettyDiagnostic)
import Fmt ((+|), (|+))
import Lex (LexM, commentL, identL, lexL, numL, opL, runLex, stringL)
import Numeric (showHex)
import Source (Source (..), newSource')
import System.FilePath ((</>))
import Test.Hspec (
    Spec,
    describe,
    it,
    parallel,
    shouldBe,
    shouldSatisfy,
 )
import Test.Hspec.QuickCheck (prop)

convert :: Source -> Either (Position, Report Text) [Token] -> Either Text [Token]
convert s x = case x of
    Left (_, err) ->
        let diag = addReport mempty err
            diag' = addFile diag s.filename s.rest
         in Left $ "" +| show (prettyDiagnostic WithUnicode (TabSize 2) diag') |+ ""
    Right tl -> Right tl

execLex :: LexM a -> Source -> Either Text [Token]
execLex m s = convert s $ underlay <$> runLex m s

spec :: Spec
spec = parallel $ do
    describe "identL" $ do
        let checkId s t = execLex identL (newSource' s) `shouldBe` Right [t]
        it "should lex identifier" $ do
            "_Foo" `checkId` Identifier (pack "_Foo")
            "_Foo asd00" `checkId` Identifier (pack "_Foo")
            "_bar3_ asd00" `checkId` Identifier (pack "_bar3_")
        it "should llex keywords" $ do
            "break asd00" `checkId` Break
    describe "opL" $ do
        let checkOp s t = execLex opL (newSource' s) `shouldBe` Right [t]
        it "should parse longest match" $ do
            ">" `checkOp` Gt
            ">>" `checkOp` RightShift
            "<=" `checkOp` Le
            "==" `checkOp` Eq
    describe "numL" $ do
        let checkNum s t = execLex numL (newSource' s) `shouldBe` Right [Integer t]
        prop "should lex int dec" $
            \x ->
                let x' = abs x
                 in show x' `checkNum` x'
        prop "should lex int hex 0x" $
            \x ->
                let x' = abs x
                 in ("0x" ++ showHex x' "") `checkNum` x'
        prop "should lex int hex 0X" $
            \x ->
                let x' = abs x
                 in ("0X" ++ showHex x' "") `checkNum` x'
    describe "stringL" $ do
        let checkStr s1 s2 = execLex stringL (newSource' ("\"" ++ s1 ++ "\"")) `shouldBe` Right [String $ pack s2]
        it "should lex string" $ do
            "\\n1234" `checkStr` "\n1234"
    describe "commentL" $ do
        let checkComment s = execLex commentL (newSource' s) `shouldBe` Right []
        it "should lex multiline comment" $ do
            checkComment "/* asdf */"
            checkComment "/*\n asdf\n */"
        it "should lex single line comment" $ do
            checkComment "//asds"
            checkComment "//asds\n"
    describe "lexL" $ do
        it "should lex example.cpp" $ do
            file <- liftIO $ readFile ("test" </> "resources" </> "example.cpp")
            let source = newSource' file
            convert source (underlay <$> runLex lexL source) `shouldSatisfy` isRight
