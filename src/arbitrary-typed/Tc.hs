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

checkType :: (MonadFail m, MonadIO m) => Term -> Type -> m Term
checkType t ty = runTc (zonkTerm =<< checkSigma t ty) =<< emptyEnv

inferType :: (MonadFail m, MonadIO m) => Term -> m (Term, Type)
inferType t =
        runTc
                ( do
                        (t', ty) <- inferSigma t
                        (,) <$> zonkTerm t' <*> zonkType ty
                )
                =<< emptyEnv

data Expected a = Infer (IORef a) | Check a

checkRho :: (MonadIO m, MonadFail m) => Term -> Rho -> Tc m Term
checkRho t ty = tcRho t (Check ty)

inferRho :: (MonadIO m, MonadFail m) => Term -> Tc m (Term, Rho)
inferRho t = do
        ref <- newTcRef (error "inferRho: empty result")
        t' <- tcRho t (Infer ref)
        (t',) <$> readTcRef ref

tcRho :: (MonadIO m, MonadFail m) => Term -> Expected Rho -> Tc m Term
tcRho (TmLit LUnit) exp_ty = instSigma (TyCon TUnit) exp_ty >> return (TmLit LUnit)
tcRho (TmVar n) exp_ty = do
        sigma <- lookupVarEnv n
        coer <- instSigma sigma exp_ty
        return $ coer `appCoer` TmVar n
tcRho (TmApp fun arg) exp_ty = do
        (fun', fun_ty) <- inferRho fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        arg' <- checkSigma arg arg_ty
        coer <- instSigma res_ty exp_ty
        return $ coer `appCoer` TmApp fun' arg'
tcRho (TmAbs var Nothing body) (Check exp_ty) = do
        (arg_ty, res_ty) <- unifyFun exp_ty
        body' <- extendVarEnv var arg_ty (checkSigma body res_ty)
        return $ TmAbs var (Just arg_ty) body'
tcRho (TmAbs var (Just arg_ty) body) (Check exp_ty) = do
        body' <- extendVarEnv var arg_ty (checkSigma body exp_ty)
        return $ TmAbs var (Just arg_ty) body'
tcRho (TmAbs var Nothing body) (Infer ref) = do
        arg_ty <- newTyVar
        (body', body_ty) <- extendVarEnv var arg_ty (inferSigma body)
        writeTcRef ref (TyFun arg_ty body_ty)
        return $ TmAbs var (Just arg_ty) body'
tcRho (TmAbs var (Just arg_ty) body) (Infer ref) = do
        (body', body_ty) <- extendVarEnv var arg_ty (inferSigma body)
        writeTcRef ref (TyFun arg_ty body_ty)
        return $ TmAbs var (Just arg_ty) body'
tcRho (TmTApp body tys) (Check rho) = do
        (body', sigma) <- inferSigma body
        sigma' <- apply sigma tys
        coer <- subsCheckRho sigma' rho
        return $ coer `appCoer` body'
tcRho (TmTApp body tys) (Infer ref) = do
        (body', sigma) <- inferSigma body
        sigma' <- apply sigma tys
        (coer, rho) <- instantiate sigma'
        writeTcRef ref rho
        return $ coer `appCoer` TmTApp body' tys
tcRho (TmTAbs tvs body) exp_ty = do
        (body', sigma) <- extendTyvarEnv tvs $ inferSigma body
        ftvs <- mapM (const newTyVar) tvs
        coer <- instSigma sigma exp_ty
        return $ coer `appCoer` substTerm tvs ftvs body'

inferSigma :: (MonadIO m, MonadFail m) => Term -> Tc m (Term, Sigma)
inferSigma (TmTAbs tvs body) = do
        (body', sigma) <- extendTyvarEnv tvs $ inferSigma body
        return (TmTAbs tvs body', TyAll tvs sigma)
inferSigma t = do
        (t, rho) <- inferRho t
        (tvs, sigma) <- generalize rho
        return (genTrans tvs `appCoer` t, sigma)

checkSigma :: (MonadIO m, MonadFail m) => Term -> Sigma -> Tc m Term
checkSigma (TmTAbs tvs body) sigma = do
        (body', body_ty) <- extendTyvarEnv tvs $ inferSigma body
        sigma' <- apply sigma (map TyVar tvs)
        coer <- subsCheck sigma' body_ty
        return $ coer `appCoer` TmTAbs tvs body'
checkSigma t sigma = do
        (coer, sktvs, rho) <- skolemise sigma
        t' <- checkRho t rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) sktvs
        unless (null bad_tvs) $ failTc "Type not polymorphic enough"
        return $ (coer <> genTrans sktvs) `appCoer` t'

subsCheck :: (MonadIO m, MonadFail m) => Sigma -> Sigma -> Tc m Coercion
subsCheck sigma1 sigma2 = do
        (coer1, sktvs, rho2) <- skolemise sigma2
        coer2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList sktvs `S.intersection` esc_tvs
        unless (null bad_tvs) $ failTc $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]
        return $ deepskolTrans sktvs coer1 coer2

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