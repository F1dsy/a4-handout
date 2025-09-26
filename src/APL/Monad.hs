{-# LANGUAGE InstanceSigs #-}

module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    evalPrint,
    catch,
    failure,
    evalKvGet,
    evalKvPut,
    transaction,
    looping,
    breakLoop,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  -- f :: (a -> b)
  -- g :: e (Free e a)
  -- Free g :: Free(e (Free e a))
  -- (fmap f) :: (Fa -> Fb) -> (Free e a -> Free e b)
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure :: Functor e => a -> Free e a
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  -- g :: e (Free e a)
  -- f :: a -> Free e b
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | PrintOp String a
  | ErrorOp Error
  | TryCatchOp (EvalM Val) (EvalM Val) (Val -> a)
  | KvGetOp Val (Val -> a)
  | KvPutOp Val Val a
  | TransactionOp (EvalM Val) (Val -> a)

instance Functor EvalOp where
  fmap :: (a -> b) -> EvalOp a -> EvalOp b
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (TryCatchOp m1 m2 k) = TryCatchOp m1 m2 $ f . k
  fmap f (KvGetOp key k) = KvGetOp key $ f . k
  fmap f (KvPutOp key val k) = KvPutOp key val $ f k
  fmap f (TransactionOp m k) = TransactionOp m $ f . k

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects ::
  (Functor e, Functor h) =>
  (e (Free e a) -> h (Free e a)) ->
  Free e a ->
  Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()

failure :: String -> EvalM a
failure = Free . ErrorOp

catch :: EvalM Val -> EvalM Val -> EvalM Val
catch m1 m2 = Free $ TryCatchOp m1 m2 pure

evalKvGet :: Val -> EvalM Val
evalKvGet key = Free $ KvGetOp key pure

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut key val = Free $ KvPutOp key val $ pure ()

transaction :: EvalM Val -> EvalM Val
transaction v = Free $ TransactionOp v pure

-- | Enclose a computation @m@ such that if a 'breakLoop' is executed in @m@,
-- execution will return here.
looping :: EvalM Val -> EvalM Val
looping = error "TODO"

-- | Return the provided value from the most immediately enclosing 'looping'.
breakLoop :: Val -> EvalM a
breakLoop = error "TODO"
