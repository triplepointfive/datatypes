{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module NoneTypeArithmetic where

import Control.Exception (catch, throw, Exception, evaluate)
import Data.Typeable (Typeable)
import Control.DeepSeq (($!!), NFData)
import GHC.Generics (Generic)

data NoRuleApplies = NoRuleApplies deriving (Show, Typeable, Exception)

data Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    deriving (Show, NFData, Generic)

isNumericVal :: Term -> Bool
isNumericVal t = case t of
    TmZero -> True
    TmSucc t1 -> isNumericVal t1
    TmPred t1 -> isNumericVal t1
    _ -> False

isVal :: Term -> Bool
isVal TmTrue  = True
isVal TmFalse = True
isVal t = isNumericVal t

eval1 :: Term -> Term
eval1 (TmIf TmTrue t2 _)      = t2
eval1 (TmIf TmFalse _ t3)     = t3
eval1 (TmIf t1 t2 t3)         = let t1' = eval1 t1 in TmIf t1' t2 t3
eval1 (TmSucc t1)             = let t1' = eval1 t1 in TmSucc t1'
eval1 (TmPred TmZero)         = TmZero
eval1 (TmPred (TmSucc nv1))   | isNumericVal nv1 = nv1
eval1 (TmPred t1)             = let t1' = eval1 t1 in TmPred t1'
eval1 (TmIsZero TmZero)       = TmTrue
eval1 (TmIsZero (TmSucc nv1)) | isNumericVal nv1 = nv1
eval1 (TmIsZero t1)           = let t1' = eval1 t1 in TmIsZero t1'
eval1 _                       = throw NoRuleApplies

eval :: Term -> IO Term
eval t = (let t' = eval1 t in eval $!! t') `catch` (\ NoRuleApplies -> return t)
