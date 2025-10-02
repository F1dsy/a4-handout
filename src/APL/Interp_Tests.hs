module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val, State)
eval' = runEval . eval

eval'' :: Exp -> ([String], Either Error Val)
eval'' e = let (l, r, _) = runEval . eval $ e in (l, r)

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
          @?= ([], Right [("x", ValInt 1)], stateInitial),
      --
      testCase "Let" $
        eval'' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval''
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right (), stateInitial),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!", stateInitial),
      --
      testCase "Div0" $
        eval'' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "TryCatch Catch" $
        eval'' (TryCatch (CstInt 0 `Eql` CstBool True) (Div (CstInt 4) (CstInt 2)))
          @?= ([], Right $ ValInt 2),
      --
      testCase "TryCatch Catch Fail" $
        eval'' (TryCatch (CstInt 0 `Eql` CstBool True) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      --
      testCase "Transaction Good" $
        eval'' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction Bad" $
        eval'' (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (KvGet (CstInt 0)))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction Bad Propagate" $
        eval'' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
          @?= ([], Left "Unknown variable: die"),
      --
      -- goodPut = KvPut (CstInt 0) (CstInt 1)
      -- badPut = Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")
      -- get0 = KvGet (CstInt 0)
      testCase "Nested Transaction Good" $
        eval''
          ( Let
              "_"
              ( Transaction
                  ( Let
                      "_"
                      (KvPut (CstInt 0) (CstInt 1))
                      ( TryCatch
                          ( Transaction
                              ( Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")
                              )
                          )
                          (CstBool True)
                      )
                  )
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Nested Transaction Bad" $
        eval''
          ( Let
              "_"
              ( TryCatch
                  ( Transaction
                      ( Transaction
                          ( Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")
                          )
                      )
                  )
                  (CstBool True)
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction rollback after failed put" $
        let badAfterPut = Let "_" (KvPut (CstInt 0) (CstInt 1)) (Var "die")
            prog = Let "_" (Transaction badAfterPut) (KvGet (CstInt 0))
         in eval'' prog
              @?= ([], Left "Unknown variable: die"),
      --
      testCase "Nested Transaction rollback" $
        let innerFail = Let "_" (KvPut (CstInt 1) (CstInt 42)) (Var "die")
            outerProg =
              Let
                "_"
                ( Transaction
                    ( Let
                        "_"
                        (KvPut (CstInt 0) (CstInt 99))
                        (Transaction innerFail)
                    )
                )
                (KvGet (CstInt 0))
         in eval'' outerProg
              @?= ([], Right (ValInt 99)),
      --
      testCase "BreakLoop" $
        eval''
          ( ForLoop ("p", CstInt 0) ("i", CstInt 100) $
              Let "_" (Break (CstBool True)) (Var "i")
          )
          @?= ([], Right (ValBool True))
          --
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

      -- testCase "Missing Keys with input" $
      --  evalIO' (KvGet (CstInt 42))
      --    >>= (@?= Left "Invalid key: ValInt 42"),
      testCase "Missing keys simulated input Int" $ do
        (out, res) <-
          captureIO ["ValInt 5"] $
            evalIO' (KvGet (CstInt 0))
        out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
        res @?= Right (ValInt 5),
      --
      testCase "Missing keys simulated input Bool" $ do
        (out, res) <-
          captureIO ["ValBool True"] $
            evalIO' (KvGet (CstInt 0))
        out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
        res @?= Right (ValBool True),
      --
      testCase "Missing keys simulated string" $ do
        (out, res) <-
          captureIO ["lol"] $
            evalIO' (KvGet (CstInt 0))
        out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
        res @?= Left "Invalid value input: lol",
      -- Tror ikke vi skal have KvPutOp
      testCase "KvPutOp" $ do
        (out, res) <-
          captureIO ["lol"] $
            evalIO' (KvPut (CstInt 0) (CstInt 0))
        out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
        res @?= Left "Invalid value input: lol",
      -- lavet KvGetOp i stedet
      testCase "KvGetOp" $ do
        (out, res) <-
          captureIO [" ValInt 1"] $
            runEvalIO $
              Free $
                KvGetOp (ValInt 0) $
                  \val -> pure val
        out @?= ["Invalid key: ValInt 0. Enter a replacement: "]
        res @?= Right (ValInt 1),
      --
      testCase "TryCatch Catch" $ do
        (out, res) <-
          captureIO [] $ evalIO' (TryCatch (CstInt 0 `Eql` CstBool True) (Div (CstInt 4) (CstInt 2)))
        (out, res) @?= ([], Right $ ValInt 2),
      --
      testCase "TryCatch Catch Fail" $ do
        (out, res) <-
          captureIO [] $ evalIO' (TryCatch (CstInt 0 `Eql` CstBool True) (Div (CstInt 1) (CstInt 0)))
        (out, res) @?= ([], Left "Division by zero"),
      --
      testCase "Transaction Good" $ do
        (out, res) <-
          captureIO [] $
            evalIO' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
        (out, res) @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction Bad" $ do
        (out, res) <-
          captureIO [] $ evalIO' (TryCatch (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))) (KvGet (CstInt 0)))
        (out, res) @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction Bad Propagate" $ do
        (out, res) <-
          captureIO [] $ evalIO' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
        (out, res) @?= ([], Left "Unknown variable: die")
        --
    ]
