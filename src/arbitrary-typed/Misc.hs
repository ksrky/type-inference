{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Misc where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Monad
import Syntax

zonkType :: (MonadIO m) => Type -> Tc m Type
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

zonkTerm :: (MonadIO m) => Term -> Tc m Term
zonkTerm (TmLit l) = return (TmLit l)
zonkTerm (TmVar n) = return (TmVar n)
zonkTerm (TmApp fun arg) = TmApp <$> zonkTerm fun <*> zonkTerm arg
zonkTerm (TmAbs var mbty body) = TmAbs var <$> zonkType `traverse` mbty <*> zonkTerm body
zonkTerm (TmTApp body ty_args) = TmTApp body <$> mapM zonkType ty_args
zonkTerm (TmTAbs ty_vars body) = TmTAbs ty_vars <$> zonkTerm body

getEnvTypes :: (Monad m) => Tc m [Type]
getEnvTypes = asks $ M.elems . tc_varenv

getMetaTvs :: (MonadIO m) => Type -> Tc m (S.Set MetaTv)
getMetaTvs ty = do
        ty' <- zonkType ty
        return (metaTvs ty')

metaTvs :: Type -> S.Set MetaTv
metaTvs TyVar{} = S.empty
metaTvs TyCon{} = S.empty
metaTvs (TyFun arg res) = metaTvs arg `S.union` metaTvs res
metaTvs (TyAll _ ty) = metaTvs ty
metaTvs (TyMeta tv) = S.singleton tv

getFreeTvs :: (MonadIO m) => Type -> Tc m (S.Set TyVar)
getFreeTvs ty = do
        ty' <- zonkType ty
        return (freeTvs ty')

freeTvs :: Type -> S.Set TyVar
freeTvs (TyVar tv) = S.singleton tv
freeTvs TyCon{} = S.empty
freeTvs (TyFun arg res) = freeTvs arg `S.union` freeTvs res
freeTvs (TyAll tvs ty) = S.fromList tvs `S.union` freeTvs ty
freeTvs TyMeta{} = S.empty

substType :: [TyVar] -> [Type] -> Type -> Type
substType tvs tys ty = apply (M.fromList (zip tvs tys)) ty
    where
        apply :: M.Map TyVar Tau -> Type -> Type
        apply s ty@(TyVar tv) = M.findWithDefault ty tv s
        apply _ ty@TyCon{} = ty
        apply s (TyFun ty1 ty2) = TyFun (apply s ty1) (apply s ty2)
        apply s (TyAll tvs t) = TyAll tvs $ apply (foldr M.delete s tvs) t
        apply _ ty@TyMeta{} = ty

substTerm :: [TyVar] -> [Type] -> Term -> Term
substTerm tvs tys t = apply (M.fromList (zip tvs tys)) t
    where
        apply :: M.Map TyVar Tau -> Term -> Term
        apply _ t@TmLit{} = t
        apply _ t@TmVar{} = t
        apply s (TmApp t1 t2) = TmApp (apply s t1) (apply s t2)
        apply s (TmAbs var mbty t) = TmAbs var (substType tvs tys <$> mbty) (apply s t)
        apply s (TmTApp t tys') = TmTApp (apply s t) (substType tvs tys <$> tys')
        apply s (TmTAbs tvs' t) = TmTAbs tvs' $ apply (foldr M.delete s tvs') t