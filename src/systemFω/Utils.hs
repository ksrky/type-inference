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
zonkType (TyApp fun arg) = do
        fun' <- zonkType fun
        arg' <- zonkType arg
        return (TyApp fun' arg')
zonkType (TyAbs x ty) = do
        ty' <- zonkType ty
        return (TyAbs x ty')
zonkType (TyMeta tv) = do
        mb_ty <- readMetaTv tv
        case mb_ty of
                Nothing -> return (TyMeta tv)
                Just ty -> do
                        ty' <- zonkType ty
                        writeMetaTv tv ty'
                        return ty'

zonkTerm :: MonadIO m => Term -> Tc m Term
zonkTerm (TmLit l) = return (TmLit l)
zonkTerm (TmVar n) = return (TmVar n)
zonkTerm (TmApp fun arg) = TmApp <$> zonkTerm fun <*> zonkTerm arg
zonkTerm (TmAbs var mty body) = TmAbs var <$> zonkType `traverse` mty <*> zonkTerm body
zonkTerm (TmPAbs pat mty body) = TmPAbs pat <$> zonkType `traverse` mty <*> zonkTerm body
zonkTerm (TmLet var rhs body) = TmLet var <$> zonkTerm rhs <*> zonkTerm body
zonkTerm (TmTApp body ty_args) = TmTApp body <$> mapM zonkType ty_args
zonkTerm (TmTAbs ty_vars body) = TmTAbs ty_vars <$> zonkTerm body

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
metaTvs (TyApp fun arg) = metaTvs fun `S.union` metaTvs arg
metaTvs (TyAbs _ ty) = metaTvs ty
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
freeTvs (TyApp fun arg) = freeTvs fun `S.union` freeTvs arg
freeTvs (TyAbs _ ty) = freeTvs ty
freeTvs TyMeta{} = S.empty