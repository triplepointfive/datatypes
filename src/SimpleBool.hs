{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
module LambdaCalculus where

import Control.Exception (catch, throw, Exception, evaluate)
import Data.Typeable (Typeable)
import Control.DeepSeq (($!!), NFData)
import GHC.Generics (Generic)

data NoRuleApplies = NoRuleApplies deriving (Show, Typeable, Exception)

data Info = Info
    deriving (Show, NFData, Generic)

data Ty
    = TyBool
    | TyArr Ty Ty
    deriving (Show, NFData, Generic)

data Binding
    = NameBinding
    | VarBind Ty
    deriving Show

type Context = [(String, Binding)]

data Term
    = TmTrue Info
    | TmFalse Info
    | TmIf Info Term Term Term
    | TmVar Info Int Int
    | TmAbs Info String Ty Term
    | TmApp Info Term Term
    deriving (Show, NFData, Generic)

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind):ctx

getTypeFromContext :: Info -> Context -> Int -> Ty
getTypeFromContext fi ctx i = case getBinding fi ctx i of
    VarBind ty -> ty
    _ -> error $ "getTypeFromContext: Wrong kind of binding for variable"
        ++ index2name fi ctx i

index2name :: Info -> Context -> Int -> String
index2name _ ctx i = fst (ctx !! i)

getBinding :: Info -> Context -> Int -> Binding
getBinding _ ctx i = snd (ctx !! i)
