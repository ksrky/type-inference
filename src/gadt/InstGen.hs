module InstGen where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Coercion
import Misc
import Monad
import Subst
import Syntax

-- | Instantiation
instantiate :: MonadIO m => Sigma -> Tc m (Coercion, Rho)
instantiate (TyAll tvs tau) = do
        tys <- mapM (const newTyVar) tvs
        return (instTrans tys, subst tvs tys tau)
instantiate ty = return (Id, ty)

skolemise :: MonadIO m => Sigma -> Tc m (Coercion, [TyVar], Rho)
skolemise (TyAll tvs ty) = do
        sks1 <- mapM newSkolemTyVar tvs
        (coercion, sks2, ty') <- skolemise (subst tvs (map TyVar sks1) ty)
        return (prpolyTrans sks1 coercion, sks1 ++ sks2, ty')
skolemise (TyFun arg_ty res_ty) = do
        (coercion, sks, res_ty') <- skolemise res_ty
        return (prfunTrans sks arg_ty coercion, sks, TyFun arg_ty res_ty')
skolemise ty = return (Id, [], ty)

-- | Generalization
generalize :: MonadIO m => Rho -> Tc m ([TyVar], Sigma)
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        quantify (S.toList all_tvs) ty

quantify :: MonadIO m => [MetaTv] -> Rho -> Tc m ([TyVar], Sigma)
quantify [] ty = return ([], ty)
quantify tvs ty = do
        let new_bndrs = take (length tvs) allBinders
        zipWithM_ writeMetaTv tvs (map TyVar new_bndrs)
        ty' <- zonkType ty
        return (new_bndrs, TyAll new_bndrs ty')

allBinders :: [TyVar]
allBinders =
        [BoundTv [x] | x <- ['a' .. 'z']]
        ++ [BoundTv (x : show i) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]