{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module NameSpec (spec) where

import Ast.Ast (Ast, Phase (Named))
import Ast.Global (Global)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Error.Diagnose (TabSize (TabSize), WithUnicode (WithoutUnicode), addFile, addReport, prettyDiagnostic)
import Fmt ((+|), (|+))
import Lex (lexL, runLex)
import NameAnalysis.Global (resolveGlbs)
import NameAnalysis.Types (runNameAnalysis)
import Parse.Global (globalsP)
import Parse.Types (runParse)
import Source (Source (..), newSource)
import System.FilePath ((</>))
import System.IO (readFile')
import Test.Hspec (Spec, describe, it, parallel, shouldSatisfy)

resolve :: [Char] -> Either Text [Ast Named (Global Named)]
resolve str =
    let src = newSource "test" str
        resultL = runLex lexL src
        resultP = resultL >>= runParse globalsP src
        resultN = resultP >>= runNameAnalysis resolveGlbs
     in case resultN of
            Left (_, err) ->
                let diag = addReport mempty err
                    diag' = addFile diag src.filename src.rest
                 in Left $ "" +| show (prettyDiagnostic WithoutUnicode (TabSize 2) diag') |+ ""
            Right a -> Right a
spec :: Spec
spec = parallel $ do
    describe "resolveGlbs" $ do
        it "should allow declare" $ do
            resolve "extern int i; int i = 0;" `shouldSatisfy` isRight
            resolve "extern int i; extern int i;" `shouldSatisfy` isRight
            resolve "extern void f(int i, int j); void f(int i, int j){}" `shouldSatisfy` isRight
        it "should disallow redefine" $ do
            resolve "void f(){int i=0; int i=0;}" `shouldSatisfy` isLeft
            resolve "void f(){int i=0; {int i=0;}}" `shouldSatisfy` isRight
            resolve "void f(){} int f = 0;" `shouldSatisfy` isLeft
            resolve "void f(){} extern int f;" `shouldSatisfy` isLeft
        it "should disallow duplicated parameters" $ do
            resolve "extern f(int i, int i);" `shouldSatisfy` isLeft
            resolve "void f(int i){int i=0;}" `shouldSatisfy` isLeft
        it "should disallow use before declare" $ do
            resolve "extern int i; void f(){i = 1+i;}" `shouldSatisfy` isRight
            resolve "void f(){i = 1;}" `shouldSatisfy` isLeft
            resolve "void f(){f();}" `shouldSatisfy` isRight
            resolve "void f(){int i=i+1;}" `shouldSatisfy` isRight
            resolve "void f(){g();}" `shouldSatisfy` isLeft
        it "should resolve parent scope variable" $ do
            resolve "void f(){int i=0;{int j = 0;{int k=0;{k = i+j;}}}}" `shouldSatisfy` isRight
        it "should resolve example.cpp" $ do
            str <- liftIO $ readFile' ("test" </> "resources" </> "example.cpp")
            resolve str `shouldSatisfy` isRight
