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

eval :: Env -> Exp -> Value
eval env (Lit i) = IntVal i
eval env (Var n) = fromJust (Map.lookup n env)
eval env (Plus a b) =
    let IntVal a' = eval env a
        IntVal b' = eval env b
    in IntVal (a' + b')
eval env (Abs n e) = FunVal env n e
eval env (App a b) =
    case a' of
        FunVal env' n body -> eval (Map.insert n b' env') body
  where
    a' = eval env a
    b' = eval env b
