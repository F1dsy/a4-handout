module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

data BreakOrError = Error Error | Break Val

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  r <- runEvalIO' envEmpty dbFile evalm
  pure $ case r of
    Left (Error err) -> Left err
    Left (Break _) -> Left "BreakLoopOp outside of loop"
    Right r' -> Right r'
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either BreakOrError a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left (Error e)
    runEvalIO' r db (Free (TryCatchOp m1 m2 k)) = do
      a1 <- runEvalIO' r db m1
      a2 <- runEvalIO' r db m2
      case a1 of
        Left (Error _) -> runEvalIO' r db $
          case a2 of
            Left (Error err) -> failure err
            Left (Break v) -> k v
            Right x2 -> k x2
        Left (Break x) -> runEvalIO' r db $ k x
        Right x -> runEvalIO' r db $ k x
    runEvalIO' r db (Free (KvGetOp key k)) = do
      s <- readDB db
      res <- case s of
        (Left err) -> pure $ failure err
        (Right state) ->
          let v = lookup key state
           in case v of
                Just x -> pure $ k x
                Nothing -> do
                  x <- prompt $ "Invalid key: " ++ show key ++ ". Enter a replacement: "
                  maybe (pure $ failure $ "Invalid value input: " ++ x) (pure . k) (readVal x)

      runEvalIO' r db res
    runEvalIO' r s (Free (KvPutOp key val m)) = do
      db <- readDB s
      case db of
        Left err -> runEvalIO' r s $ failure err
        Right state -> do
          let state' = (key, val) : state
          writeDB s state'
          runEvalIO' r s m
    runEvalIO' r s (Free (TransactionOp m k)) =
      withTempDB
        ( \s' -> do
            copyDB s s'
            res <- runEvalIO' r s' m
            case res of
              Right v -> do
                copyDB s' s
                runEvalIO' r s $ k v
              Left (Error err) -> do pure $ Left (Error err)
              Left (Break v) -> do pure $ Left (Break v)
        )
    runEvalIO' r s (Free (LoopOp m k)) =
      do
        res <- runEvalIO' r s m
        case res of
          Right v -> runEvalIO' r s (k v)
          Left (Break v) -> runEvalIO' r s (k v)
          Left (Error err) -> pure $ Left (Error err)
    runEvalIO' _ _ (Free (BreakLoopOp v)) = pure $ Left (Break v)