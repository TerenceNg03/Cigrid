{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AsmSpec (spec) where

import Asm.RegAlloc (allocReg)
import Asm.RegSpill (spill)
import Control.Monad (forM_)
import Data.Text (Text, pack)
import Error.Diagnose (TabSize (TabSize), WithUnicode (WithoutUnicode), addFile, addReport, prettyDiagnostic)
import Fmt ((+|), (|+))
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import IR.Global (genGlbs)
import IR.Types (runIRM)
import Lex (lexL, runLex)
import NameAnalysis.Global (resolveGlbs)
import NameAnalysis.Types (runNameAnalysis)
import Parse.Global (globalsP)
import Parse.Types (runParse)
import Source (Source (..), newSource)
import System.FilePath ((-<.>), (</>))
import System.IO (readFile')
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
import TypeCheck.Global (globalsTC)
import TypeCheck.Types (runType)

execProcess :: FilePath -> [String] -> IO ()
execProcess p args = do
    (code, stdout, stderr) <- readProcessWithExitCode p args ""
    case code of
        ExitSuccess -> return ()
        ExitFailure i ->
            error $
                "Command \""
                    +| p
                    |+ " "
                    +| unwords args
                    |+ "\" exit with code "
                    +| i
                    |+ "\n\n"
                        <> "stdout:\n"
                    +| stdout
                    |+ "\nstderr:\n"
                    +| stderr
                    |+ ""

testFileExpect' :: FilePath -> Maybe Text -> IO ()
testFileExpect' path text = do
    content <- readFile' path
    let src = newSource path content
        resultL = runLex lexL src
        resultP = resultL >>= runParse globalsP src
        resultN = resultP >>= runNameAnalysis resolveGlbs
        resultT = resultN >>= runType globalsTC
        resultIR = resultT >>= runIRM src genGlbs
        resultAsm = spill . allocReg <$> resultIR
        asm = case resultAsm of
            Left (_, err) -> do
                let diag = addFile mempty src.filename src.rest
                    diag' = addReport diag err
                error $ show $ prettyDiagnostic WithoutUnicode (TabSize 2) diag'
            Right asm' -> show asm'
    writeFile (path -<.> "asm") asm
    execProcess "nasm" ["-felf64", path -<.> "asm", "-o", path -<.> "o"]
    execProcess "gcc" ["-no-pie", path -<.> "o", "-o", path -<.> "out"]
    output <- readProcess (path -<.> "out") [] ""
    forM_ text (shouldBe $ pack output)

testFileExpect :: Text -> FilePath -> IO ()
testFileExpect t = flip testFileExpect' (Just t)

testFile :: FilePath -> IO ()
testFile = flip testFileExpect' Nothing

spec :: Spec
spec = parallel $ do
    let base = "test" </> "resources"
    describe "Code Emission" $ do
        it "should gen basic" $
            testFile (base </> "basic.cpp")
        it "should gen arithmetic" $ do
            testFile (base </> "arithmetic.cpp")
        it "should gen compare" $ do
            testFile (base </> "compare.cpp")
        it "should gen bitop" $ do
            testFile (base </> "bitop.cpp")
        it "should gen if" $ do
            testFile (base </> "if1.cpp")
            testFile (base </> "if2.cpp")
            testFile (base </> "if3.cpp")
        it "should gen while" $ do
            testFile (base </> "while1.cpp")
            testFile (base </> "while2.cpp")
        it "should gen call" $ do
            testFile (base </> "call.cpp")
            testFile (base </> "abs.cpp")
        it "should gen alloc" $ do
            testFile (base </> "alloc.cpp")
            "1, 3, 44, 93, 123, \n" `testFileExpect` (base </> "alloc2.cpp")
            "Hello World\n" `testFileExpect` (base </> "alloc3.cpp")
        it "should ignore printf" $ do
            testFile (base </> "printf.cpp")
        it "should gen general program" $ do
            testFile (base </> "complete1.cpp")
