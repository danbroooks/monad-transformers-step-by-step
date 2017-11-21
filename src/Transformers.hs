module Transformers where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map                   as Map
import           Data.Maybe

type Name = String

data Exp
    = Lit Integer
    | Var Name
    | Plus Exp
           Exp
    | Abs Name
          Exp
    | App Exp
          Exp
    deriving (Show)

data Value
    = IntVal Integer
    | FunVal Env
             Name
             Exp
    deriving (Show)

type Env = Map.Map Name Value

type Eval a = ExceptT String Identity a

runEval :: Eval a -> Either String a
runEval = runIdentity . runExceptT

eval :: Monad m => Env -> Exp -> ExceptT String m Value
eval env (Lit i) = return $ IntVal i
eval env (Var n) =
    case Map.lookup n env of
        Just n' -> return n'
        Nothing -> throwError ("undefined variable: " ++ n)
eval env (Plus a b) = do
    ia <- eval env a
    ib <- eval env b
    case (ia, ib) of
        (IntVal a', IntVal b') -> return $ IntVal (a' + b')
        _                      -> throwError "type error in addition"
eval env (Abs n e) = return $ FunVal env n e
eval env (App a b) = do
    a' <- eval env a
    b' <- eval env b
    case a' of
        FunVal env' n body -> eval (Map.insert n b' env') body
        _                  -> throwError "type error in application"
