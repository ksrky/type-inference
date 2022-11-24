module Utils where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Monad
import Syntax

zonkType :: MonadIO m => Type -> Infer m Type
zonkType (TyVar tv) = return (TyVar tv)
zonkType (TyCon tc) = return (TyCon tc)
zonkType (TyFun arg res) = do
        arg' <- zonkType arg
        res' <- zonkType res
        return (TyFun arg' res')
zonkType (TyAll tvs ty) = do
        ty' <- zonkType ty
        return (TyAll tvs ty')
zonkType (TyMeta tv) = do
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (TyMeta tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'

getEnvTypes :: Monad m => Infer m [Type]
getEnvTypes = asks M.elems

getMetaTvs :: MonadIO m => Type -> Infer m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonkType ty
        return (metaTvs ty')

getFreeTyVars :: MonadIO m => Type -> Infer m (S.Set TyVar)
getFreeTyVars ty = do
        ty' <- zonkType ty
        return (freeTyVars ty')

metaTvs :: Type -> S.Set MetaTv
metaTvs TyVar{} = S.empty
metaTvs TyCon{} = S.empty
metaTvs (TyFun arg res) = metaTvs arg `S.union` metaTvs res
metaTvs (TyAll _ ty) = metaTvs ty
metaTvs (TyMeta tv) = S.singleton tv

freeTyVars :: Type -> S.Set TyVar
freeTyVars (TyVar tv) = S.singleton tv
freeTyVars TyCon{} = S.empty
freeTyVars (TyFun arg res) = freeTyVars arg `S.union` freeTyVars res
freeTyVars (TyAll tvs ty) = freeTyVars ty `S.difference` S.fromList tvs
freeTyVars TyMeta{} = S.empty