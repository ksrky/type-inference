module InstGen where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Misc
import Monad
import Subst
import Syntax

-- | Instantiation
instantiate :: MonadIO m => Sigma -> Tc m Rho
instantiate (TyAll tvs tau) = do
        tys <- mapM (const newTyVar) tvs
        return $ subst tvs tys tau
instantiate ty = return ty

skolemise :: MonadIO m => Sigma -> Tc m ([TyVar], Rho)
skolemise (TyAll tvs ty) = do
        sks1 <- mapM newSkolemTyVar tvs
        (sks2, ty') <- skolemise (subst tvs (map TyVar sks1) ty)
        return (sks1 ++ sks2, ty')
skolemise (TyFun arg_ty res_ty) = do
        (sks, res_ty') <- skolemise res_ty
        return (sks, TyFun arg_ty res_ty')
skolemise ty = return ([], ty)

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