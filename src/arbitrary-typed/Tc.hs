{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-} 

module Tc where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Set as S
import Prettyprinter

import Coercion
import Misc
import Monad
import Syntax
import Unify
import InstGen

data Expected a = Infer (IORef a) | Check a


tcRho :: (MonadIO m, MonadFail m) => Term  -> Expected Rho -> Tc m Term
tcRho (TmLit LUnit) exp_ty = instSigma (TyCon TUnit) exp_ty >> return (TmLit LUnit)
tcRho (TmVar n) exp_ty = do
        sigma <- lookupEnv n
        coer <- instSigma sigma exp_ty
        return $ unCoer coer $ TmVar n
tcRho _ _ = undefined

subsCheck :: (MonadIO m, MonadFail m) => Sigma -> Sigma -> Tc m Coercion
subsCheck sigma1 sigma2 = do
        (coer1, skol_tvs, rho2) <- skolemise sigma2
        coer2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList skol_tvs `S.intersection` esc_tvs
        unless (null bad_tvs) $ failTc $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]
        return $ deepskolTrans skol_tvs coer1 coer2

subsCheckRho :: (MonadIO m, MonadFail m) => Sigma -> Rho -> Tc m Coercion
subsCheckRho sigma1@TyAll{} rho2 = do
        (coer1, rho1) <- instantiate sigma1
        coer2 <- subsCheckRho rho1 rho2
        return (coer2 <> coer1)
subsCheckRho rho1 (TyFun a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 a2 r2
subsCheckRho (TyFun a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun a1 r1 a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return Id

subsCheckFun :: (MonadIO m, MonadFail m) => Sigma -> Rho -> Sigma -> Rho -> Tc m Coercion
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return $ funTrans a2 co_arg co_res


instSigma :: (MonadIO m, MonadFail m) => Sigma -> Expected Rho -> Tc m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coer, rho) <- instantiate sigma
        writeTcRef r rho
        return coer