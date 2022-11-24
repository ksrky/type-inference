module Infer where

import Control.Monad.Reader
import qualified Data.Set as S

import Monad
import Subst
import Syntax
import Unify

inferType :: MonadFail m => Term -> m Type
inferType t = fst <$> runInfer (inferSigma t) emptyEnv 0

-- | Infernece of Tau
inferTau :: MonadFail m => Term -> Infer m Tau
inferTau t = do
        exp_ty <- newTyVar
        s <- checkTau t exp_ty
        return $ apply s exp_ty

checkTau :: MonadFail m => Term -> Tau -> Infer m Subst
checkTau (TmLit LUnit) exp_ty = unify exp_ty (TyCon TUnit)
checkTau (TmVar n) exp_ty = do
        sigma <- lookupEnv n
        tau <- instantiate sigma
        unify exp_ty tau
checkTau (TmApp fun arg) exp_ty = do
        arg_ty <- newTyVar
        s1 <- checkTau fun (TyFun arg_ty exp_ty)
        s2 <- applyEnv s1 $ checkTau arg (apply s1 arg_ty)
        return $ s2 `compose` s1
checkTau (TmAbs var body) exp_ty = do
        var_ty <- newTyVar
        body_ty <- newTyVar
        s1 <- unify exp_ty (TyFun var_ty body_ty)
        s2 <- applyEnv s1 $ extendEnv var (apply s1 var_ty) $ checkTau body (apply s1 body_ty)
        return $ s2 `compose` s1
checkTau (TmLet var rhs body) exp_ty = do
        var_ty <- inferSigma rhs
        extendEnv var var_ty $ checkTau body exp_ty

-- | Inference of Sigma
inferSigma :: MonadFail m => Term -> Infer m Sigma
inferSigma t = do
        tau <- inferTau t
        generalize tau

-- | Generalization and Instantiation
instantiate :: Monad m => Sigma -> Infer m Tau
instantiate (TyAll ns ty) = do
        ns' <- mapM (const newTyVar) ns
        let s = newSubst ns ns'
        return $ apply s ty
instantiate ty = return ty

generalize :: Monad m => Tau -> Infer m Sigma
generalize ty = do
        env <- ask
        let ns = S.toList $ ftv ty `S.difference` ftvEnv env
        return $ if null ns then ty else TyAll ns ty
