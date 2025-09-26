module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch" $
        runEval (catch (failure "Oh no!") (pure $ ValInt 1))
          @?= ([], Right $ ValInt 1),
      --
      testCase "TryCatch2" $
        eval' (TryCatch (CstInt 5) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Right $ ValInt 5),
      --
      testCase "TryCatch Bad" $
        eval' (TryCatch (CstInt 0 `Eql` CstBool True) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      --
      testCase "Transaction Good" $
        eval' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction Bad" $
        eval' (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (KvGet (CstInt 0)))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction Bad Propagate" $
        eval' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
          @?= ([], Left "Unknown variable: die")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $
        do
          let s1 = "Lalalalala"
              s2 = "Weeeeeeeee"
          (out, res) <-
            captureIO [] $
              runEvalIO $ do
                evalPrint s1
                evalPrint s2
          (out, res) @?= ([s1, s2], Right ()),
      testCase
        "TryCatch2"
        $ eval' (TryCatch (CstInt 5) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Right $ ValInt 5),
      testCase "TryCatch Bad" $
        eval' (TryCatch (CstInt 0 `Eql` CstBool True) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      testCase "Write DB" $
        do
          x <- evalIO' (KvPut (CstInt 0) (CstInt 1))
          pure undefined

          -- NOTE: This test will give a runtime error unless you replace the
          -- version of `eval` in `APL.Eval` with a complete version that supports
          -- `Print`-expressions. Uncomment at your own risk.
          -- testCase "print 2" $ do
          --    (out, res) <-
          --      captureIO [] $
          --        evalIO' $
          --          Print "This is also 1" $
          --            Print "This is 1" $
          --              CstInt 1
          --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]
