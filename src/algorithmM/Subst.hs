module Subst where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Syntax

type Subst = M.Map Uniq Tau

emptySubst :: Subst
emptySubst = M.empty

newSubst :: [Uniq] -> [Type] -> Subst
newSubst = (M.fromList .) . zip

-- | apply substitution to types
apply :: Subst -> Type -> Type
apply s t@(TyVar n) = M.findWithDefault t n s
apply _ (TyCon tc) = TyCon tc
apply s (TyFun ty1 ty2) = TyFun (apply s ty1) (apply s ty2)
apply s (TyAll ns t) = TyAll ns $ apply (foldr M.delete s ns) t

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

-- | get free type variables
ftv :: Type -> S.Set Uniq
ftv (TyVar u) = S.singleton u
ftv TyCon{} = S.empty
ftv (TyFun ty1 ty2) = ftv ty1 `S.union` ftv ty2
ftv (TyAll ns ty) = ftv ty `S.difference` S.fromList ns

ftvEnv :: Env -> S.Set Uniq
ftvEnv env = mconcat (map ftv (M.elems env))