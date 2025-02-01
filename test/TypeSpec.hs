{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeSpec (spec) where

import Ast.Ast (Ast, Phase (Typed))
import Ast.Global (Global)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isRight)
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
import TypeCheck.Global (globalsTC)
import TypeCheck.Types (runType)

checkType :: [Char] -> Either Text [Ast Typed (Global Typed)]
checkType str =
    let src = newSource "test" str
        resultL = runLex lexL src
        resultP = resultL >>= runParse globalsP src
        resultN = resultP >>= runNameAnalysis resolveGlbs
        resultT = resultN >>= runType globalsTC
     in case resultT of
            Left (_, err) ->
                let diag = addReport mempty err
                    diag' = addFile diag src.filename src.rest
                 in Left $ "" +| show (prettyDiagnostic WithoutUnicode (TabSize 2) diag') |+ ""
            Right a -> Right a
spec :: Spec
spec = parallel $ do
    describe "globalsTC" $ do
        it "should resolve example.cpp" $ do
            str <- liftIO $ readFile' ("test" </> "resources" </> "example.cpp")
            checkType str `shouldSatisfy` isRight
