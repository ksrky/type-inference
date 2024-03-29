{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tc where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Set as S
import Prettyprinter

import Coercion
import InstGen
import Misc
import Monad
import Syntax
import Unify

checkType :: (MonadFail m, MonadIO m) => Term 'In -> Type -> m (Term 'Out)
checkType t ty = runTc (checkSigma t ty) =<< emptyEnv

inferType :: (MonadFail m, MonadIO m) => Term 'In -> m (Term 'Out, Type)
inferType t = runTc (inferSigma t) =<< emptyEnv

data Expected a = Infer (IORef a) | Check a

-- | Type check of Rho
checkRho :: (MonadIO m, MonadFail m) => Term 'In -> Rho -> Tc m (Term 'Out)
checkRho t ty = tcRho t (Check ty) >>= zonkTerm

inferRho :: (MonadIO m, MonadFail m) => Term 'In -> Tc m (Term 'Out, Rho)
inferRho t = do
        ref <- newTcRef (error "inferRho: empty result")
        t <- tcRho t (Infer ref) >>= zonkTerm
        (t,) <$> readTcRef ref

tcRho :: (MonadIO m, MonadFail m) => Term 'In -> Expected Rho -> Tc m (Term 'Out)
tcRho (TmLit LUnit) exp_ty = instSigma (TyCon TUnit) exp_ty >> return (TmLit LUnit)
tcRho (TmVar n) exp_ty = do
        sigma <- lookupEnv n
        coer <- instSigma sigma exp_ty
        return $ unCoer coer $ TmVar n
tcRho (TmApp fun arg) exp_ty = do
        (fun', fun_ty) <- inferRho fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        arg' <- checkSigma arg arg_ty
        coer <- instSigma res_ty exp_ty
        return $ unCoer coer $ TmApp fun' arg'
tcRho (TmAbs var body) (Check exp_ty) = do
        (arg_ty, res_ty) <- unifyFun exp_ty
        body' <- extendEnv var arg_ty (checkRho body res_ty)
        return $ TmAbs' var arg_ty body'
tcRho (TmAbs var body) (Infer ref) = do
        var_ty <- newTyVar
        (body', body_ty) <- extendEnv var var_ty (inferRho body)
        writeTcRef ref (TyFun var_ty body_ty)
        return $ TmAbs' var var_ty body'
tcRho (TmLet var rhs body) exp_ty = do
        (rhs', var_ty) <- inferSigma rhs
        body' <- extendEnv var var_ty $ tcRho body exp_ty
        return $ TmLet' var var_ty rhs' body'

-- | Type check of Sigma
inferSigma :: (MonadFail m, MonadIO m) => Term 'In -> Tc m (Term 'Out, Sigma)
inferSigma t = do
        (t, rho) <- inferRho t
        (tvs, sigma) <- generalize rho
        t' <- zonkTerm t -- reduce TyMeta
        return (unCoer (genTrans tvs) t', sigma)

checkSigma :: (MonadIO m, MonadFail m) => Term 'In -> Sigma -> Tc m (Term 'Out)
checkSigma t sigma = do
        (coer, sktvs, rho) <- skolemise sigma
        t' <- checkRho t rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) sktvs
        unless (null bad_tvs) $ failTc "Type not polymorphic enough"
        return $ unCoer (coer <> genTrans sktvs) t'

-- | Subsumption checking
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

-- | Instantiation of Sigma
instSigma :: (MonadIO m, MonadFail m) => Sigma -> Expected Rho -> Tc m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coer, rho) <- instantiate sigma
        writeTcRef r rho
        return coer