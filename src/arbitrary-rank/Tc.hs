{-# LANGUAGE OverloadedStrings #-}

module Tc where

import Control.Monad.Reader
import qualified Data.Set as S

import Data.IORef
import Prettyprinter

import InstGen
import Monad
import Syntax
import Unify
import Misc

checkType :: (MonadFail m, MonadIO m) => Term -> Type -> m ()
checkType t ty = runTc (checkSigma t ty) =<< emptyEnv

inferType :: (MonadFail m, MonadIO m) => Term -> m Type
inferType t = runTc (inferSigma t) =<< emptyEnv

data Expected a = Infer (IORef a) | Check a

-- | Type check of Rho
checkRho :: (MonadIO m, MonadFail m) => Term -> Rho -> Tc m ()
checkRho t ty = tcRho t (Check ty)

inferRho :: (MonadIO m, MonadFail m) => Term -> Tc m Rho
inferRho t = do
        ref <- newTcRef (error "inferRho: empty result")
        _ <- tcRho t (Infer ref)
        readTcRef ref

tcRho :: (MonadIO m, MonadFail m) => Term -> Expected Rho -> Tc m ()
tcRho (TmLit LUnit) exp_ty = instSigma (TyCon TUnit) exp_ty
tcRho (TmVar n) exp_ty = do
        sigma <- lookupEnv n
        instSigma sigma exp_ty
tcRho (TmApp fun arg) exp_ty = do
        fun_ty <- inferRho fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        checkSigma arg arg_ty
        instSigma res_ty exp_ty
tcRho (TmAbs var body) (Check exp_ty) = do
        (var_ty, body_ty) <- unifyFun exp_ty
        extendEnv var var_ty (checkRho body body_ty)
tcRho (TmAbs var body) (Infer ref) = do
        var_ty <- newTyVar
        body_ty <- extendEnv var var_ty (inferRho body)
        writeTcRef ref (TyFun var_ty body_ty)
tcRho (TmLet var rhs body) exp_ty = do
        var_ty <- inferSigma rhs
        extendEnv var var_ty $ tcRho body exp_ty

-- | Type check of Sigma
inferSigma :: (MonadFail m, MonadIO m) => Term -> Tc m Sigma
inferSigma t = do
        rho <- inferRho t
        generalize rho

checkSigma :: (MonadIO m, MonadFail m) => Term -> Sigma -> Tc m ()
checkSigma t sigma = do
        (skol_tvs, rho) <- skolemise sigma
        checkRho t rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        unless (null bad_tvs) $ failTc "Type not polymorphic enough"

-- | Subsumption checking
subsCheck :: (MonadIO m, MonadFail m) => Sigma -> Sigma -> Tc m ()
subsCheck sigma1 sigma2 = do
        (skol_tvs, rho2) <- skolemise sigma2
        subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList skol_tvs `S.intersection` esc_tvs
        unless (null bad_tvs) $ failTc $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]

subsCheckRho :: (MonadIO m, MonadFail m) => Sigma -> Rho -> Tc m ()
subsCheckRho sigma1@TyAll{} rho2 = do
        rho1 <- instantiate sigma1
        subsCheckRho rho1 rho2
subsCheckRho rho1 (TyFun a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 a2 r2
subsCheckRho (TyFun a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun a1 r1 a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2

subsCheckFun :: (MonadIO m, MonadFail m) => Sigma -> Rho -> Sigma -> Rho -> Tc m ()
subsCheckFun a1 r1 a2 r2 = do
        subsCheck a2 a1
        subsCheckRho r1 r2

-- | Instantiation of Sigma
instSigma :: (MonadIO m, MonadFail m) => Sigma -> Expected Rho -> Tc m ()
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        rho <- instantiate sigma
        writeTcRef r rho