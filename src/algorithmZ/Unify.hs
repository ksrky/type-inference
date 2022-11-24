{-# LANGUAGE OverloadedStrings #-}

module Unify where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S
import Prettyprinter

import Monad
import Syntax
import Utils

unify :: (MonadFail m, MonadIO m) => Tau -> Tau -> Infer m ()
unify (TyVar n1) (TyVar n2) | n1 == n2 = return ()
unify (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
unify (TyFun arg1 res1) (TyFun arg2 res2) = do
        unify arg1 arg2
        unify res1 res2
unify (TyMeta tv1) (TyMeta tv2) | tv1 == tv2 = return ()
unify (TyMeta tv) ty = unifyVar tv ty
unify ty (TyMeta tv) = unifyVar tv ty
unify ty1 ty2 = failInf $ hsep ["Cannot unify types:", squotes $ pretty ty1, "with", squotes $ pretty ty2]

unifyVar :: (MonadFail m, MonadIO m) => MetaTv -> Tau -> Infer m ()
unifyVar tv1 ty2@(TyMeta tv2) = do
        mb_ty1 <- readMetaTv tv1
        mb_ty2 <- readMetaTv tv2
        case (mb_ty1, mb_ty2) of
                (Just ty1, _) -> unify ty1 ty2
                (Nothing, Just ty2) -> unify (TyMeta tv1) ty2
                (Nothing, Nothing) -> writeMetaTv tv1 ty2
unifyVar tv1 ty2 = do
        occursCheck tv1 ty2
        writeMetaTv tv1 ty2

occursCheck :: (MonadFail m, MonadIO m) => MetaTv -> Tau -> Infer m ()
occursCheck tv1 ty2 = do
        tvs2 <- getMetaTvs ty2
        when (tv1 `S.member` tvs2) $ failInf $ hsep ["Infinite type:", squotes $ pretty ty2]