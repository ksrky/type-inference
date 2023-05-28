module Subst where

import qualified Data.Map.Strict as M

import Syntax

type Subst = M.Map TyVar Tau

subst :: [TyVar] -> [Type] -> Type -> Type
subst tvs tys ty = let s = M.fromList (zip tvs tys) in apply s ty

apply :: Subst -> Type -> Type
apply s ty@(TyVar tv) = M.findWithDefault ty tv s
apply _ ty@TyCon{} = ty
apply s (TyFun ty1 ty2) = TyFun (apply s ty1) (apply s ty2)
apply s (TyAll tvs ty) = TyAll tvs $ apply (foldr M.delete s tvs) ty
apply s (TyApp ty1 ty2) = TyApp (apply s ty1) (apply s ty2)
apply s (TyAbs x ty) = TyAbs x (apply s ty)
apply _ ty@TyMeta{} = ty