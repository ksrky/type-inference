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
inferTau (TmLit LUnit) = return $ TyCon TUnit
inferTau (TmVar n) = do
        sigma <- lookupEnv n
        instantiate sigma
inferTau (TmApp fun arg) = do
        fun_ty <- inferTau fun
        arg_ty <- inferTau arg
        res_ty <- newTyVar
        unify fun_ty (TyFun arg_ty res_ty)
        return res_ty
inferTau (TmAbs var body) = do
        var_ty <- newTyVar
        body_ty <- extendEnv var var_ty (inferTau body)
        return $ TyFun var_ty body_ty
inferTau (TmLet var rhs body) = do
        var_ty <- inferTau rhs
        var_sigma <- generalize var_ty
        extendEnv var var_sigma (inferTau body)

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