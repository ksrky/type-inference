module Subst where

import qualified Data.Map.Strict as M

import Syntax

type Subst = M.Map TyVar Tau

emptySubst :: Subst
emptySubst = M.empty

newSubst :: [TyVar] -> [Type] -> Subst
newSubst = (M.fromList .) . zip

-- | apply substitution to types
apply :: Subst -> Type -> Type
apply s ty@(TyVar tv) = M.findWithDefault ty tv s
apply _ ty@TyCon{} = ty
apply s (TyFun ty1 ty2) = TyFun (apply s ty1) (apply s ty2)
apply s (TyAll tvs t) = TyAll tvs $ apply (foldr M.delete s tvs) t
apply _ ty@TyMeta{} = ty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1