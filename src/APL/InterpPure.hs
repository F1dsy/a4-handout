module APL.InterpPure (runEval) where

import APL.Monad

data BreakOrError = Error Error | Break Val

runEval :: EvalM a -> ([String], Either Error a)
runEval ex =
  let (l, r, _) = runEval' envEmpty stateInitial ex
   in case r of
        Left (Error err) -> (l, Left err)
        Left (Break _) -> (l, Left "BreakLoopOp outside of loop")
        Right r' -> (l, Right r')
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either BreakOrError a, State)
    runEval' _ s (Pure x) = ([], pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res, s') = runEval' r s m
       in (p : ps, res, s')
    runEval' _ s (Free (ErrorOp e)) = ([], Left $ Error e, s)
    runEval' r s (Free (TryCatchOp m1 m2 k)) =
      case runEval' r s m1 of
        (_, Left (Error _), s1) -> runEval' r s $
          case runEval' r s1 m2 of
            (_, res, _) ->
              case res of
                Left (Error err) -> failure err
                Left (Break v) -> k v
                Right v -> k v
        (_, Left (Break x), s1) -> runEval' r s1 $ k x
        (_, Right x, s1) -> runEval' r s1 $ k x
    runEval' r s (Free (KvGetOp key k)) =
      runEval' r s (let v = lookup key s in maybe (failure $ "Invalid key: " ++ show key) k v)
    runEval' r s (Free (KvPutOp key val m)) = runEval' r ((key, val) : s) m
    runEval' r s (Free (TransactionOp m k)) =
      case runEval' r s m of
        (logs, Right v, s1) ->
          let (logs', res', s2) = runEval' r s1 (k v)
           in case res' of
                Right v' -> (logs ++ logs', Right v', s2)
                Left err -> (logs ++ logs', Left err, s)
        (logs, Left err, _) ->
          (logs, Left err, s)
    runEval' r s (Free (LoopOp m k)) =
      let (logs, res, s') = runEval' r s m
       in case res of
            Right x ->
              -- body completed without error: loop again
              let (logs', res', s'') = runEval' r s' (k x)
               in case res' of
                    Left (Break v') -> (logs ++ logs', Left (Break v'), s'')
                    Left err -> (logs ++ logs', Left err, s)
                    Right _ -> runEval' r s'' (Free (LoopOp m k))
            Left (Error err) ->
              runEval' r s' $ failure err
            Left (Break v) ->
              (logs, Left (Break v), s')
    runEval' _ s (Free (BreakLoopOp v)) = ([], Left (Break v), s)
