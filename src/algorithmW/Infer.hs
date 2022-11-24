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
inferTau :: MonadFail m => Term -> Infer m (Subst, Tau)
inferTau (TmLit LUnit) = return (emptySubst, TyCon TUnit)
inferTau (TmVar n) = do
        sigma <- lookupEnv n
        tau <- instantiate sigma
        return (emptySubst, tau)
inferTau (TmApp fun arg) = do
        (s1, fun_ty) <- inferTau fun
        (s2, arg_ty) <- applyEnv s1 (inferTau arg)
        res_ty <- newTyVar
        s3 <- unify (apply s2 fun_ty) (TyFun arg_ty res_ty)
        return (s3 `compose` s2 `compose` s1, apply s3 res_ty)
inferTau (TmAbs var body) = do
        var_ty <- newTyVar
        (s1, body_ty) <- extendEnv var var_ty (inferTau body)
        return (s1, TyFun (apply s1 var_ty) body_ty)
inferTau (TmLet var rhs body) = do
        (s1, var_ty) <- inferTau rhs
        (s2, body_ty) <- applyEnv s1 $ do
                var_sigma <- generalize var_ty
                extendEnv var var_sigma (inferTau body)
        return (s2 `compose` s1, body_ty)

-- | Inference of Sigma
inferSigma :: MonadFail m => Term -> Infer m Sigma
inferSigma t = do
        (_, tau) <- inferTau t
        generalize tau

-- | Generalization and Instantiation
instantiate :: Monad m => Sigma -> Infer m Tau
instantiate (TyAll ns ty) = do
        tvs <- mapM (const newTyVar) ns
        let s = newSubst ns tvs
        return $ apply s ty
instantiate ty = return ty

generalize :: Monad m => Tau -> Infer m Sigma
generalize ty = do
        env <- ask
        let ns = S.toList $ ftv ty `S.difference` ftvEnv env
        return $ if null ns then ty else TyAll ns ty