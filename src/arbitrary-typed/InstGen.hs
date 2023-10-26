module InstGen where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Coercion
import Misc
import Monad
import Syntax

-- | Instantiation
instantiate :: (MonadIO m) => Sigma -> Tc m (Coercion, Rho)
instantiate (TyAll tvs rho) = do
        tys <- mapM (const newTyVar) tvs
        return (instTrans tys, substType tvs tys rho)
instantiate ty = return (Id, ty)

skolemise :: (MonadIO m) => Sigma -> Tc m (Coercion, [TyVar], Rho)
skolemise (TyAll tvs rho) = do
        sks1 <- mapM newSkolemTyVar tvs
        (coer, sks2, rho') <- skolemise (substType tvs (map TyVar sks1) rho)
        return (prpolyTrans sks1 coer, sks1 ++ sks2, rho')
skolemise (TyFun arg_ty res_ty) = do
        (coer, sks, res_ty') <- skolemise res_ty
        return (prfunTrans sks arg_ty coer, sks, TyFun arg_ty res_ty')
skolemise ty = return (Id, [], ty)

-- | Generalization
generalize :: (MonadIO m) => Rho -> Tc m ([TyVar], Sigma)
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        quantify (S.toList all_tvs) ty

quantify :: (MonadIO m) => [MetaTv] -> Rho -> Tc m ([TyVar], Sigma)
quantify [] rho = return ([], rho)
quantify tvs rho = do
        let new_bndrs = take (length tvs) allBinders
        zipWithM_ writeMetaTv tvs (map TyVar new_bndrs)
        rho' <- zonkType rho
        return (new_bndrs, TyAll new_bndrs rho')

allBinders :: [TyVar]
allBinders = [BoundTv ('_' : show i) | i <- [1 :: Integer ..]]

-- | Application
apply :: (MonadFail m) => Sigma -> [Tau] -> m Sigma
apply sigma [] = return sigma
apply (TyAll tvs rho) tys | length tvs == length tys = return (substType tvs tys rho)
apply (TyAll tvs rho) tys | length tvs > length tys = do
        let (tvs1, tvs2) = splitAt (length tys) tvs
        return $ TyAll tvs2 (substType tvs1 tys rho)
apply _ _ = fail "too much type application"