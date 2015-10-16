{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module LambdaCalculus where

import Control.Exception (catch, throw, Exception, evaluate)
import Data.Typeable (Typeable)
import Control.DeepSeq (($!!), NFData)
import GHC.Generics (Generic)

import Text.Printf (printf)

data NoRuleApplies = NoRuleApplies deriving (Show, Typeable, Exception)

type Binding = String

type Context = [Binding]

data Term
    = TmVar Int Int
    | TmAbs String Term
    | TmApp Term Term
    deriving (Show, NFData, Generic)

printtm :: Context -> Term -> String
printtm ctx (TmApp t1 t2) = printf "(%s %s)" (printtm ctx t1) (printtm ctx t2)
printtm ctx (TmVar x n)   = if length ctx == n then ctx !! x else "[bad index]"
printtm ctx (TmAbs x t1)  = printf "(λ%s.%s)" x' (printtm ctx' t1)
  where (ctx', x') = pickfreshname ctx x

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx x | x `elem` ctx = pickfreshname ctx nx
                    | otherwise = (x:ctx, x)
  where nx = x ++ "'"

c2 = TmAbs "s" $ TmAbs "z" $ TmApp (TmVar 1 1) (TmApp (TmVar 1 1) (TmVar 0 1))
fix = TmAbs "f" (TmApp g g)
  where
    g = TmAbs "x" $ TmApp (TmVar 1 2) (TmAbs "y" (TmApp (TmApp (TmVar 1 3) (TmVar 1 3)) (TmVar 0 3)))


