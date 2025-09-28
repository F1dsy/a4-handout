module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (_, Left _) -> runEval' r s $ case runEval' r s m2 of (_, res) -> either failure k res
        (_, Right x) -> runEval' r s $ k x
    runEval' r s (Free (KvGetOp key k)) = runEval' r s (let v = lookup key s in maybe (failure $ "Invalid key: " ++ show key) k v)
    runEval' r s (Free (KvPutOp key val m)) = runEval' r ((key, val) : s) m
    -- runEval' r s (Free (TransactionOp m k)) =
    --  runEval' r s $ do
    --    x <- m
    --    k x
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (logs, Right v) ->
          let (logs', res') = runEval' r s (k v)
           in (logs ++ logs', res')
        (logs, Left err) ->
          (logs, Left err)
    -- runEval' r s (Free (TransactionOp m k)) =
    --  case runEval'' r s m of
    --    -- success: commit (keep new state) and continue with k v
    --    (logs, Right v, s') ->
    --      let (_, Right v, s') = runEval' r s' (k v)
    --       in (logs ++ logs', res')
    --    -- failure: rollback (restore original state)
    --    (_, Left err) -> runEval'' r s $ failure err
    --  where
    --    runEval'' :: Env -> State -> EvalM a -> ([String], Either Error a, State)
    --    runEval'' _ s' (Pure x) = ([], pure x, s')
    --    runEval'' r' s' m' = let (logs, res, s'') = runEval'' r' s' m' in (logs, res, s'')

    runEval' r s (Free (LoopOp m k)) =
      case runEval' r s m of
        (_, Left "BreakLoop") -> runEval' r s undefined
        (_, Left _) -> runEval' r s undefined
        (_, Right x) -> runEval' r s $ k x
    runEval' _ _ (Free (BreakLoopOp v)) = ([], Left "Break outside loop")

-- let oldState = s
--  in runEval' r s $
--       let (str, res) = runEval' r s m
--        in case res of
--             Left err -> Free (ErrorOp err)
--             Right val -> k val
