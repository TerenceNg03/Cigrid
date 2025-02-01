{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseSpec (spec) where

import Ast.AstSrc (AstSrc (AstSrc))
import Ast.BOp (BOp (Div, MinusB, Mul, Plus, RightShift))
import Ast.CType (CType (..))
import Ast.Expr (Expr (..))
import Ast.UOp (UOp (MinusU))
import Data.Either (isRight)
import Data.Text (Text)
import Error.Diagnose (Position (Position), TabSize (TabSize), WithUnicode (WithoutUnicode), addFile, addReport, prettyDiagnostic)
import Fmt ((+|), (|+))
import Lex (lexL, runLex)
import Parse.CTypes (typeP)
import Parse.Expr (exprP)
import Parse.Global (globalsP)
import Parse.Stmt (stmtP)
import Parse.Types (ParseM, runParse)
import Source (Source (filename, rest), newSource)
import Test.Hspec (Spec, describe, it, parallel, shouldBe, shouldSatisfy)

parse :: ParseM a -> String -> Either Text a
parse m str = do
    let s = newSource "test" str
        result = runLex lexL s >>= runParse m s
    case result of
        Left (_, err) ->
            let diag = addReport mempty err
                diag' = addFile diag s.filename s.rest
             in Left $ "" +| show (prettyDiagnostic WithoutUnicode (TabSize 2) diag') |+ ""
        Right a -> Right a

spec :: Spec
spec = parallel $ do
    let b = AstSrc $ Position (1, 1) (1, 1) ""
    describe "typeP" $ do
        let checkType str x = parse typeP str `shouldBe` Right (b x)
        it "should parse type" $ do
            "int" `checkType` TInt
            "char" `checkType` TChar
            "int**" `checkType` TPoint (b $ TPoint $ b TInt)
            "void" `checkType` TVoid
    describe "exprP" $ do
        let checkExpr str x = parse (exprP 0) str `shouldBe` Right (b x)
        it "should parse simple expression" $ do
            "\"I am a String\"" `checkExpr` EString "I am a String"
            "'c'" `checkExpr` EChar 'c'
            "0x1234" `checkExpr` EInt 0x1234
            "name" `checkExpr` EVar "name"
        it "should handle precedence" $ do
            "1 + 2 + 3"
                `checkExpr` EBinOp
                    (b Plus)
                    (b $ EBinOp (b Plus) (b $ EInt 1) (b $ EInt 2))
                    (b $ EInt 3)
            "1 + 2 * 3"
                `checkExpr` EBinOp
                    (b Plus)
                    (b $ EInt 1)
                    (b $ EBinOp (b Mul) (b $ EInt 2) (b $ EInt 3))
            " 1 / (5 >> 6)"
                `checkExpr` EBinOp
                    (b Div)
                    (b $ EInt 1)
                    (b $ EBinOp (b RightShift) (b $ EInt 5) (b $ EInt 6))
            "1 - -2"
                `checkExpr` EBinOp
                    (b MinusB)
                    (b $ EInt 1)
                    (b $ EUnOp (b MinusU) (b $ EInt 2))
        it "should parse function call" $ do
            "f(1,\"2\",3)" `checkExpr` ECall (b "f") [b $ EInt 1, b $ EString "2", b $ EInt 3]
            "func2()" `checkExpr` ECall (b "func2") []
        it "should parse array access" $ do
            "arr[112].xyz" `checkExpr` EArrayAccess (b "arr") (b $ EInt 112) (Just $ b "xyz")
            "arr2['c']" `checkExpr` EArrayAccess (b "arr2") (b $ EChar 'c') Nothing
        it "should parse new" $ do
            "new int[10]" `checkExpr` ENew (b TInt) (b $ EInt 10)
            "new void**[11]"
                `checkExpr` ENew
                    (b $ TPoint (b $ TPoint $ b TVoid))
                    (b $ EInt 11)
    describe "stmtP" $ do
        let checkStmt str = parse stmtP str `shouldSatisfy` isRight
        it "should parse break" $ do
            checkStmt "break;"
        it "should parse if" $ do
            checkStmt "if (x+10) break;"
            checkStmt "if (x+10) break; else break;"
        it "should parse scope" $ do
            checkStmt "{break; break;}"
            checkStmt "{}"
            checkStmt "{x = x+100;}"
        it "should parse while" $ do
            checkStmt "while(1)break;"
            checkStmt "while(1){x=x+1;break;}"
        it "should parse delete" $ do
            checkStmt "delete []x__0;"
        it "should parse return" $ do
            checkStmt "return ;"
            checkStmt "return new int[0];"
        it "should parse assign" $ do
            checkStmt "x = 1;"
            checkStmt "x[0] = 1;"
            checkStmt "x[0].xx = 1;"
            checkStmt "x[0].xx++;"
            checkStmt "x[0].xx--;"
            checkStmt "x++;"
            checkStmt "y++;"
            checkStmt "int **x = 10;"
            checkStmt "int **x = new int*[15+3];"
        it "should parse for" $ do
            checkStmt "for(int i=0;i<10;i++)show(i);"
    describe "globalP" $ do
        let checkGlb str = parse globalsP str `shouldSatisfy` isRight
        it "should parse struct" $ do
            checkGlb "struct s1 {};"
            checkGlb "struct s1 {int x;};"
            checkGlb "struct s1 {int x;int y;};"
        it "should parse extern var" $ do
            checkGlb "extern int x;"
        it "should parse extern function" $ do
            checkGlb "extern int** f();"
            checkGlb "extern int** f(void x, void* y, char z);"
            checkGlb "extern int** f(void x);"
        it "should parse var" $ do
            checkGlb "int x = 1;"
        it "should parse function" $ do
            checkGlb "int** f(){}"
            checkGlb "int** f(void x, void* y, char z){return null;}"
            checkGlb "int** f(void x){return new int*[10];}"
