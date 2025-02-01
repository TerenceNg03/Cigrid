{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PrettySpec (spec) where

import Ast.Ast (IsAst (underlay), Phase (Parsed))
import Ast.Global (Global)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isSpace)
import Error.Diagnose (TabSize (TabSize), WithUnicode (WithUnicode), addFile, addReport, prettyDiagnostic)
import Fmt ((+|), (|+))
import Lex (lexL, runLex)
import Parse.Global (globalsP)
import Parse.Types (runParse)
import Prettyprinter (pretty)
import Source (Source (..), newSource)
import System.FilePath ((</>))
import System.IO (readFile')
import Test.Hspec (Spec, describe, expectationFailure, it, parallel, shouldBe)

spec :: Spec
spec = parallel $ do
    describe "Pretty" $ do
        it "should print examples.txt from examples.cpp" $ do
            file <- liftIO $ readFile' ("test" </> "resources" </> "example.cpp")
            target <- liftIO $ readFile' ("test" </> "resources" </> "examples.txt")
            let s = newSource "example.cpp" file
                result = runLex lexL s >>= runParse globalsP s
            case result of
                Left (_, err) ->
                    let diag = addReport mempty err
                        diag' = addFile diag s.filename s.rest
                     in expectationFailure $ "" +| show (prettyDiagnostic WithUnicode (TabSize 2) diag') |+ ""
                Right gs ->
                    let output = concatMap (show . pretty . underlay @_ @(Global Parsed)) gs
                     in filter (not . isSpace) output `shouldBe` filter (not . isSpace) target
