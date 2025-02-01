{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Asm.Liveness (analysisIR, prettyIRLiveness)
import Asm.RegAlloc (allocReg, prettyIRInterGraph)
import Asm.RegSpill (spill)
import Ast.Ast (IsAst (underlay), Phase (Named, Parsed))
import Ast.Global (Global)
import Control.Monad (forM_, when)
import Error.Diagnose (Position (Position), TabSize (TabSize), WithUnicode (WithoutUnicode), addFile, addReport, defaultStyle, printDiagnostic, stderr, stdout)
import IR.Global (genGlbs)
import IR.Types (runIRM)
import Lex (lexL, runLex)
import NameAnalysis.Global (resolveGlbs)
import NameAnalysis.Types (runNameAnalysis)
import Options.Applicative (ParserInfo, argument, execParser, fullDesc, helper, info, long, metavar, str, switch, (<**>))
import Parse.Global (globalsP)
import Parse.Types (runParse)
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)
import Source (Source (filename, rest), newSource)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPrint, readFile')
import TypeCheck.Global (globalsTC)
import TypeCheck.Types (runType)

data Args = Args
    { prettyPrint :: Bool
    , lineError :: Bool
    , nameAnalysis :: Bool
    , typeCheck :: Bool
    , debugResolved :: Bool
    , filename :: FilePath
    , asm :: Bool
    , liveness :: Bool
    , inferGraph :: Bool
    , regAlloc :: Bool
    }

argsOpt :: ParserInfo Args
argsOpt = info (args <**> helper) fullDesc
  where
    args =
        Args
            <$> switch (long "pretty-print")
            <*> switch (long "line-error")
            <*> switch (long "name-analysis")
            <*> switch (long "type-check")
            <*> switch (long "debug-resolved")
            <*> argument str (metavar "FILENAME")
            <*> switch (long "asm")
            <*> switch (long "liveness")
            <*> switch (long "interference")
            <*> switch (long "regalloc")

main :: IO ()
main = do
    args <- execParser argsOpt
    content <- readFile' args.filename
    let src = newSource args.filename content
        resultL = runLex lexL src
        resultP = resultL >>= runParse globalsP src
        resultN = resultP >>= runNameAnalysis resolveGlbs
        resultT = resultN >>= runType globalsTC
        resultIR = resultT >>= runIRM src genGlbs
        resultAsm = spill <$> resultIR
        resultAllocedAsm = spill . allocReg <$> resultIR
        handleErr (Position (ln, _) _ _) err code = do
            let diag = addFile mempty src.filename src.rest
                diag' = addReport diag err
            when args.lineError $ hPrint stderr ln
            printDiagnostic stdout WithoutUnicode (TabSize 2) defaultStyle diag'
            exitWith $ ExitFailure code
    case resultP of
        Left (sp, err) -> handleErr sp err 1
        Right p ->
            when args.prettyPrint $
                forM_ p $
                    putDoc . pretty . underlay @_ @(Global Parsed)
    when args.nameAnalysis $ do
        case resultN of
            Left (sp, err) -> handleErr sp err 2
            Right p ->
                when args.debugResolved $
                    forM_ p $
                        putDoc . pretty . underlay @_ @(Global Named)
    when args.typeCheck $ do
        case resultT of
            Left (sp, err) -> handleErr sp err 2
            Right _ -> return ()
    when (args.asm && not args.regAlloc) $ do
        case resultAsm of
            Left (sp, err) -> handleErr sp err 2
            Right asmDoc -> putDoc asmDoc
    when args.liveness $ do
        case resultIR of
            Left (sp, err) -> handleErr sp err 2
            Right ir -> putDoc $ prettyIRLiveness ir $ analysisIR ir
    when args.inferGraph $ do
        case resultIR of
            Left (sp, err) -> handleErr sp err 2
            Right ir -> putDoc $ prettyIRInterGraph ir
    when (args.asm && args.regAlloc) $ do
        case resultAllocedAsm of
            Left (sp, err) -> handleErr sp err 2
            Right asmDoc -> putDoc asmDoc
