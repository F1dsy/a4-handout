module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a, State)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a, State)
    runEval' _ s (Pure x) = ([], pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res, s') = runEval' r s m
      in (p : ps, res, s')
    runEval' _ s (Free (ErrorOp e)) = ([], Left e, s)

    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (_, Left _, s1) -> runEval' r s $ 
          case runEval' r s1 m2 of
            (_, res, _) -> either failure k res
        (_, Right x, s1) -> runEval' r s1 $ k x
    runEval' r s (Free (KvGetOp key k)) = runEval' r s (
      let v = lookup key s 
      in maybe (failure $ "Invalid key: " ++ show key) k v)
    runEval' r s (Free (KvPutOp key val m)) = runEval' r ((key, val) : s) m
    -- runEval' r s (Free (TransactionOp m k)) =
    --  runEval' r s $ do
    --    x <- m
    --    k x
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (logs, Right v, s1) ->
          let (logs', res', s2) = runEval' r s1 (k v)
           in case res' of 
               Right v' -> (logs ++ logs', Right v', s2)
               Left err -> (logs ++ logs', Left err, s)
        (logs, Left err, _) ->
          (logs, Left err, s)

    runEval' r s (Free (LoopOp m k)) = undefined
    runEval' _ _ (Free (BreakLoopOp v)) = undefined
    --runEval' r s (Free (LoopOp m k)) =
    --  case runEval' r s m of
    --    (_, Left "BreakLoop") -> runEval' r s undefined
    --    (_, Left _) -> runEval' r s undefined
    --    (_, Right x) -> runEval' r s $ k x
    --runEval' _ _ (Free (BreakLoopOp v)) = ([], Left "Break outside loop")

-- let oldState = s
--  in runEval' r s $
--       let (str, res) = runEval' r s m
--        in case res of
--             Left err -> Free (ErrorOp err)
--             Right val -> k val
