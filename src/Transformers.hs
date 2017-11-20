module Transformers where

import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map               as Map
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

type Eval a = Identity a

runEval :: Eval a -> a
runEval = runIdentity

eval :: Monad m => Env -> Exp -> m Value
eval env (Lit i) = return $ IntVal i
eval env (Var n) =
    case Map.lookup n env of
        Just n' -> return n'
        Nothing -> fail ("undefined variable: " ++ n)
eval env (Plus a b) = do
    IntVal a' <- eval env a
    IntVal b' <- eval env b
    return $ IntVal (a' + b')
eval env (Abs n e) = return $ FunVal env n e
eval env (App a b) = do
    a' <- eval env a
    b' <- eval env b
    case a' of
        FunVal env' n body -> eval (Map.insert n b' env') body
