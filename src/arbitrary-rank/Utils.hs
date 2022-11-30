module Utils where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Monad
import Syntax

zonkType :: MonadIO m => Type -> Tc m Type
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

getEnvTypes :: Monad m => Tc m [Type]
getEnvTypes = asks M.elems

getMetaTvs :: MonadIO m => Type -> Tc m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonkType ty
        return (metaTvs ty')

metaTvs :: Type -> S.Set MetaTv
metaTvs TyVar{} = S.empty
metaTvs TyCon{} = S.empty
metaTvs (TyFun arg res) = metaTvs arg `S.union` metaTvs res
metaTvs (TyAll _ ty) = metaTvs ty
metaTvs (TyMeta tv) = S.singleton tv

getFreeTvs :: MonadIO m => Type -> Tc m (S.Set TyVar)
getFreeTvs ty = do
        ty' <- zonkType ty
        return (freeTvs ty')

freeTvs :: Type -> S.Set TyVar
freeTvs (TyVar tv) = S.singleton tv
freeTvs TyCon{} = S.empty
freeTvs (TyFun arg res) = freeTvs arg `S.union` freeTvs res
freeTvs (TyAll tvs ty) = S.fromList tvs `S.union` freeTvs ty
freeTvs TyMeta{} = S.empty