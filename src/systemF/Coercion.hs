{-# LANGUAGE DataKinds #-}

module Coercion where

import Syntax

data Coercion = Id | Fn (Term 'Out -> Term 'Out)

instance Eq Coercion where
        Id == Id = True
        _ == _ = False

instance Semigroup Coercion where
        Id <> coer = coer
        coer <> Id = coer
        Fn f1 <> Fn f2 = Fn (f1 . f2)

instance Monoid Coercion where
        mempty = Id

unCoer :: Coercion -> Term 'Out -> Term 'Out
unCoer Id = id
unCoer (Fn f) = f

genTrans :: [TyVar] -> Coercion
genTrans [] = Id
genTrans tvs = Fn (TmTAbs tvs)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans arg_tys = Fn (`TmTApp` arg_tys)

prpolyTrans :: [TyVar] -> Coercion -> Coercion
prpolyTrans [] coer = coer
prpolyTrans tvs coer = Fn $ \e -> TmTAbs tvs (unCoer coer $ TmTApp e (map TyVar tvs))

prfunTrans :: [TyVar] -> Sigma -> Coercion -> Coercion
prfunTrans [] _ coer = coer
prfunTrans _ _ Id = Id
prfunTrans tvs arg_ty coer =
        let coer' e = unCoer coer $ TmTAbs tvs (TmApp (TmTApp e (map TyVar tvs)) (TmVar "_"))
         in Fn $ TmAbs' "_" arg_ty . coer'

deepskolTrans :: [TyVar] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 <> coer2
deepskolTrans qns coer1 coer2 = coer1 <> Fn (TmTAbs qns) <> coer2

funTrans :: Sigma -> Coercion -> Coercion -> Coercion
funTrans _ Id Id = Id
funTrans a2 co_arg co_res =
        Fn $ \f -> TmAbs' "_" a2 (unCoer co_res $ TmApp f (unCoer co_arg $ TmVar "_"))