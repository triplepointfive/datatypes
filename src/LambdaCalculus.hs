{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
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

termShift :: Int -> Term -> Term
termShift d = tmmap
    (\ c (TmVar x n) -> if x >= c then TmVar (x+d) (n+d) else TmVar x (n+d))

termSubst :: Int -> Term -> Term -> Term
termSubst j s = tmmap
    (\ c (TmVar x n) -> if x == j + c then termShift c s else TmVar x n)

tmmap :: (Int -> Term -> Term) -> Term -> Term
tmmap onvar = walk 0
  where
    walk :: Int -> Term -> Term
    walk c t@(TmVar _ _) = onvar c t
    walk c (TmAbs x t1)  = TmAbs x (walk (c+1) t1)
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Context -> Term -> Bool
isval _ (TmAbs _ _) = True
isval _ _           = False

eval1 :: Context -> Term -> Term
eval1 ctx (TmApp (TmAbs _ t12) v2) | isval ctx v2 = termSubstTop v2 t12
eval1 ctx (TmApp v1 t2) | isval ctx v1 = TmApp v1 (eval1 ctx t2)
eval1 ctx (TmApp t1 t2) = TmApp (eval1 ctx t1) t2
eval1 _ t = throw NoRuleApplies

eval :: Context -> Term -> IO Term
eval ctx t = (let t' = eval1 ctx t in eval ctx $!! t')
    `catch` (\ NoRuleApplies -> return t)

y = TmApp (TmAbs "y" $ TmApp (TmVar 0 1) (TmVar 0 1)) (TmAbs "x" $ TmVar 0 1)
b = TmApp (TmVar 0 1) (TmAbs "x" (TmAbs "y" (TmVar 2 3)))
c2 = TmAbs "s" $ TmAbs "z" $ TmApp (TmVar 1 1) (TmApp (TmVar 1 1) (TmVar 0 1))
fix = TmAbs "f" (TmApp g g)
  where
    g = TmAbs "x" $ TmApp (TmVar 1 2) (TmAbs "y" (TmApp (TmApp (TmVar 1 3) (TmVar 1 3)) (TmVar 0 3)))
