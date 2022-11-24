module Infer where

import Control.Monad.Reader
import qualified Data.Set as S

import Monad
import Subst
import Syntax
import Unify
import Utils

inferType :: (MonadFail m, MonadIO m) => Term -> m Type
inferType t = fst <$> runInfer (inferSigma t >>= zonkType) emptyEnv 0

-- | Infernece of Tau
inferTau :: (MonadFail m, MonadIO m) => Term -> Infer m Tau
inferTau t = do
        exp_ty <- newTyVar
        checkTau t exp_ty
        return exp_ty

checkTau :: (MonadFail m, MonadIO m) => Term -> Tau -> Infer m ()
checkTau (TmLit LUnit) exp_ty = unify exp_ty (TyCon TUnit)
checkTau (TmVar n) exp_ty = do
        sigma <- lookupEnv n
        tau <- instantiate sigma
        unify exp_ty tau
checkTau (TmApp fun arg) exp_ty = do
        arg_ty <- newTyVar
        checkTau fun (TyFun arg_ty exp_ty)
        checkTau arg arg_ty
checkTau (TmAbs var body) exp_ty = do
        var_ty <- newTyVar
        body_ty <- newTyVar
        unify exp_ty (TyFun var_ty body_ty)
        extendEnv var var_ty $ checkTau body body_ty
checkTau (TmLet var rhs body) exp_ty = do
        var_ty <- inferSigma rhs
        extendEnv var var_ty $ checkTau body exp_ty

-- | Inference of Sigma
inferSigma :: (MonadFail m, MonadIO m) => Term -> Infer m Sigma
inferSigma t = do
        tau <- inferTau t
        generalize tau

-- | Generalization and Instantiation
instantiate :: MonadIO m => Sigma -> Infer m Tau
instantiate (TyAll ns ty) = do
        ns' <- mapM (const newTyVar) ns
        let s = newSubst ns ns'
        return $ apply s ty
instantiate ty = return ty

generalize :: MonadIO m => Tau -> Infer m Sigma
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        if null all_tvs then return ty else quantify (S.toList all_tvs) ty

quantify :: MonadIO m => [MetaTv] -> Tau -> Infer m Sigma
quantify tvs ty = do
        let new_bndrs = take (length tvs) allBinders
        zipWithM_ writeMetaTv tvs (map TyVar new_bndrs)
        ty' <- zonkType ty
        return $ TyAll new_bndrs ty'

allBinders :: [TyVar]
allBinders =
        [BoundTv [x] | x <- ['a' .. 'z']]
        ++ [BoundTv (x : show i) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]