module Coercion where

import Syntax

data Coercion = Id | Coer (Term -> Term)

instance Eq Coercion where
        Id == Id = True
        _ == _ = False

infixr 9 .>, <.>

(.>) :: Coercion -> Term -> Term
Id .> t = t
Coer f .> t = f t

(<.>) :: Coercion -> Coercion -> Coercion
Id <.> f = f
f <.> Id = f
Coer f1 <.> Coer f2 = Coer (f1 . f2)

genTrans :: [TyVar] -> Coercion
genTrans [] = Id
genTrans tvs = Coer (TmTAbs tvs)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Coer (`TmTApp` tys)

prpolyTrans :: [TyVar] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = Coer $ \t -> TmTAbs sks1 (coercion .> TmTApp t (map TyVar sks1))

prfunTrans :: [TyVar] -> Sigma -> Coercion -> Coercion
prfunTrans [] _ coercion = coercion
prfunTrans sks _ Id = Coer $ \t -> TmTAbs sks (TmTApp t (map TyVar sks))
prfunTrans sks arg_ty coercion = Coer $ \t -> TmAbs "$x" (Just arg_ty) (coercion .> TmTAbs sks (TmApp (TmTApp t (map TyVar sks)) (TmVar "$x")))

deepskolTrans :: [TyVar] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 <.> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 <.> Coer (TmTAbs skol_tvs) <.> coer2

funTrans :: Sigma -> Coercion -> Coercion -> Coercion
funTrans _ Id Id = Id
funTrans a2 co_arg co_res = Coer $ \f -> TmAbs "$x" (Just a2) (co_res .> TmApp f (co_arg .> TmVar "$x"))