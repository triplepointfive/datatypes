{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
module SimpleBool where

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
    deriving (Show, NFData, Generic, Eq)

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

typeOf :: Context -> Term -> Ty
typeOf _ (TmTrue _)             = TyBool
typeOf _ (TmFalse _)            = TyBool
typeOf ctx (TmIf fi t1 t2 t3)   = typeOfIf ctx fi t1 t2 t3
typeOf ctx (TmVar fi i _)       = getTypeFromContext fi ctx i
typeOf ctx (TmAbs fi x tyT1 t2) = typeOfAbs fi ctx x tyT1 t2
typeOf ctx (TmApp fi t1 t2)     = typeOfApp fi ctx t1 t2

typeOfIf :: Context -> Info -> Term -> Term -> Term -> Ty
typeOfIf ctx fi t1 t2 t3
    | (/=) (typeOf ctx t1) TyBool = error $
        show fi ++ "guard of conditional not a boolean"
    | (/=) tyT2 (typeOf ctx t3)   = error $
        show fi ++ "arms of conditional have different types"
    | otherwise = tyT2
  where
    tyT2 = typeOf ctx t2

typeOfAbs :: Info -> Context -> String -> Ty -> Term -> Ty
typeOfAbs fi ctx x tyT1 t2 = TyArr tyT1 tyT2
  where
    ctx' = addBinding ctx x (VarBind tyT1)
    tyT2 = typeOf ctx' t2

typeOfApp :: Info -> Context -> Term -> Term -> Ty
typeOfApp fi ctx t1 t2 = case tyT1 of
    TyArr tyT11 tyT12 -> if (==) tyT2 tyT11 then tyT12
        else error $ show fi ++ "parameter type mismatch"
    _ -> error $ show fi ++ "arrow type expected"
  where
    tyT1 = typeOf ctx t1
    tyT2 = typeOf ctx t2
