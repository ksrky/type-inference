{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tc where

import Control.Monad.Reader
import qualified Data.Set as S

import Data.IORef
import Prettyprinter

import InstGen
import Monad
import Syntax
import Unify
import Utils

checkType :: (MonadFail m, MonadIO m) => Term -> Type -> m Term
checkType t ty = fst <$> runTc (checkSigma t ty) emptyEnv 0

inferType :: (MonadFail m, MonadIO m) => Term -> m (Term, Type)
inferType t = fst <$> runTc (inferSigma t) emptyEnv 0

data Expected a = Infer (IORef a) | Check a

-- | Type check of Rho
checkRho :: (MonadIO m, MonadFail m) => Term -> Rho -> Tc m Term
checkRho t ty = tcRho t (Check ty) >>= zonkTerm

inferRho :: (MonadIO m, MonadFail m) => Term -> Tc m (Term, Rho)
inferRho t = do
        ref <- newTcRef (error "inferRho: empty result")
        t <- tcRho t (Infer ref) >>= zonkTerm
        (t,) <$> readTcRef ref

tcRho :: (MonadIO m, MonadFail m) => Term -> Expected Rho -> Tc m Term
tcRho (TmLit LUnit) exp_ty = instSigma (TyCon TUnit) exp_ty >> return (TmLit LUnit)
tcRho (TmVar n) exp_ty = do
        sigma <- lookupEnv n
        coercion <- instSigma sigma exp_ty
        return $ coercion $ TmVar n
tcRho (TmApp fun arg) exp_ty = do
        (fun', fun_ty) <- inferRho fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        arg' <- checkSigma arg arg_ty
        coercion <- instSigma res_ty exp_ty
        return $ coercion $ TmApp fun' arg'
tcRho (TmAbs var _ body) (Check exp_ty) = do
        (var_ty, body_ty) <- unifyFun exp_ty
        body' <- extendEnv var var_ty (checkRho body body_ty)
        return $ TmAbs var (Just var_ty) body'
tcRho (TmAbs var _ body) (Infer ref) = do
        var_ty <- newTyVar
        (body', body_ty) <- extendEnv var var_ty (inferRho body)
        writeTcRef ref (TyFun var_ty body_ty)
        return $ TmAbs var (Just var_ty) body'
tcRho (TmLet var rhs body) exp_ty = do
        (rhs', var_ty) <- inferSigma rhs
        body' <- extendEnv var var_ty $ tcRho body exp_ty
        return $ TmLet var rhs' body'
tcRho _ _ = fail "Exception"

-- | Type check of Sigma
inferSigma :: (MonadFail m, MonadIO m) => Term -> Tc m (Term, Sigma)
inferSigma t = do
        (t, rho) <- inferRho t
        rho' <- generalize rho
        t' <- zonkTerm t -- reduce TyMeta
        return (t', rho')

checkSigma :: (MonadIO m, MonadFail m) => Term -> Sigma -> Tc m Term
checkSigma t sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        t' <- checkRho t rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        unless (null bad_tvs) $ failTc "Type not polymorphic enough"
        return $ coercion $ if null skol_tvs then t' else TmTAbs skol_tvs t'

-- | Subsumption checking
subsCheck :: (MonadIO m, MonadFail m) => Sigma -> Sigma -> Tc m (Term -> Term)
subsCheck sigma1 sigma2 = do
        (coercion1, skol_tvs, rho2) <- skolemise sigma2
        coercion2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList skol_tvs `S.intersection` esc_tvs
        unless (null bad_tvs) $ failTc $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]
        return $ if null skol_tvs then id else coercion1 . TmTAbs skol_tvs . coercion2

subsCheckRho :: (MonadIO m, MonadFail m) => Sigma -> Rho -> Tc m (Term -> Term)
subsCheckRho sigma1@TyAll{} rho2 = do
        (coercion1, rho1) <- instantiate sigma1
        coercion2 <- subsCheckRho rho1 rho2
        return (coercion2 . coercion1)
subsCheckRho rho1 (TyFun a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 a2 r2
subsCheckRho (TyFun a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun a1 r1 a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return id

subsCheckFun :: (MonadIO m, MonadFail m) => Sigma -> Rho -> Sigma -> Rho -> Tc m (Term -> Term)
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return $ \f -> TmAbs "?x" (Just a2) (co_res $ TmApp f (co_arg (TmVar "?x")))

-- | Instantiation of Sigma
instSigma :: (MonadIO m, MonadFail m) => Sigma -> Expected Rho -> Tc m (Term -> Term)
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coercion, rho) <- instantiate sigma
        writeTcRef r rho
        return coercion