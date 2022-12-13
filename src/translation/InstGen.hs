module InstGen where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Monad
import Subst
import Syntax
import Utils

-- | Instantiation
instantiate :: MonadIO m => Sigma -> Tc m (Coercion, Rho)
instantiate (TyAll tvs tau) = do
        tys <- mapM (const newTyVar) tvs
        let coercion = Coer (`TmTApp` tys)
        return (if null tys then Id else coercion, subst tvs tys tau)
instantiate ty = return (Id, ty)

skolemise :: MonadIO m => Sigma -> Tc m (Coercion, [TyVar], Rho)
skolemise (TyAll tvs ty) = do
        sks1 <- mapM newSkolemTyVar tvs
        (coercion, sks2, ty') <- skolemise (subst tvs (map TyVar sks1) ty)
        let coercion' = Coer $ \t -> TmTAbs sks1 (coercion @@ TmTApp t (map TyVar sks1))
        return (if null sks2 then Id else coercion', sks1 ++ sks2, ty')
skolemise (TyFun arg_ty res_ty) = do
        (coercion, sks, res_ty') <- skolemise res_ty
        let coercion' = case coercion of
                Id -> Coer $ \t -> TmTAbs sks (TmTApp t (map TyVar sks))
                _ -> Coer $ \t -> TmAbs "?x" (Just arg_ty) (coercion @@ TmTAbs sks (TmApp (TmTApp t (map TyVar sks)) (TmVar "?x")))
        return (if null sks then coercion else coercion', sks, TyFun arg_ty res_ty')
skolemise ty = return (Id, [], ty)

-- | Generalization
generalize :: MonadIO m => Rho -> Tc m Sigma
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        if null all_tvs then return ty else quantify (S.toList all_tvs) ty

quantify :: MonadIO m => [MetaTv] -> Rho -> Tc m Sigma
quantify tvs ty = do
        let new_bndrs = take (length tvs) allBinders
        zipWithM_ writeMetaTv tvs (map TyVar new_bndrs)
        ty' <- zonkType ty
        return $ TyAll new_bndrs ty'

allBinders :: [TyVar]
allBinders =
        [BoundTv [x] | x <- ['a' .. 'z']]
        ++ [BoundTv (x : show i) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]