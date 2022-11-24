module Unify where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Monad
import Subst
import Syntax

unify :: MonadFail m => Tau -> Tau -> Infer m Subst
unify (TyVar u) ty = unifyVar u ty
unify ty (TyVar u) = unifyVar u ty
unify (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return emptySubst
unify (TyFun arg1 res1) (TyFun arg2 res2) = do
        s1 <- unify arg1 arg2
        s2 <- unify res1 res2
        return (s2 `compose` s1)
unify ty1 ty2 = fail $ "Cannot unify type: '" ++ show ty1 ++ "' with '" ++ show ty2 ++ "'"

unifyVar :: MonadFail m => Uniq -> Tau -> Infer m Subst
unifyVar u ty
        | ty == TyVar u = return emptySubst
        | occursCheck u ty = fail $ "Infinite type: '" ++ show ty ++ "'"
        | otherwise = return $ M.singleton u ty

occursCheck :: Uniq -> Tau -> Bool
occursCheck u ty = u `S.member` ftv ty