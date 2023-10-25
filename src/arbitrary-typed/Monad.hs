{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Monad where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter
import Prettyprinter.Render.String

import Syntax

-- | TcEnv
data TcEnv = TcEnv
        { tc_varenv :: VarEnv
        , tc_tyvarEnv :: TyvarEnv
        , tc_uniq :: IORef Uniq
        }

-- | Tc monad
newtype Tc m a = Tc {runTc :: TcEnv -> m a}

instance (Monad m) => Functor (Tc m) where
        fmap f (Tc m) = Tc $ \env -> do
                x <- m env
                return (f x)

instance (Monad m) => Applicative (Tc m) where
        pure x = Tc (\_ -> return x)
        f <*> x = Tc $ \env -> do
                f' <- runTc f env
                runTc (fmap f' x) env

instance (Monad m) => Monad (Tc m) where
        m >>= k = Tc $ \env -> do
                v <- runTc m env
                runTc (k v) env

instance (MonadFail m) => MonadFail (Tc m) where
        fail s = Tc $ \_ -> fail s

instance MonadTrans Tc where
        lift m = Tc (const m)

instance (Monad m) => MonadReader TcEnv (Tc m) where
        ask = Tc return
        local f (Tc m) = Tc (m . f)

failTc :: (MonadFail m) => Doc ann -> Tc m a
failTc doc = fail $ renderString $ layoutPretty defaultLayoutOptions doc

-- | IORef
newTcRef :: (MonadIO m) => a -> Tc m (IORef a)
newTcRef v = lift (liftIO $ newIORef v)

readTcRef :: (MonadIO m) => IORef a -> Tc m a
readTcRef r = lift (liftIO $ readIORef r)

writeTcRef :: (MonadIO m) => IORef a -> a -> Tc m ()
writeTcRef r v = lift (liftIO $ writeIORef r v)

-- | type variable generation
newTyVar :: (MonadIO m) => Tc m Type
newTyVar = TyMeta <$> newMetaTv

newSkolemTyVar :: (MonadIO m) => TyVar -> Tc m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUniq

newMetaTv :: (MonadIO m) => Tc m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newTcRef Nothing

readMetaTv :: (MonadIO m) => MetaTv -> Tc m (Maybe Tau)
readMetaTv (MetaTv _ ref) = readTcRef ref

writeMetaTv :: (MonadIO m) => MetaTv -> Tau -> Tc m ()
writeMetaTv (MetaTv _ ref) ty = writeTcRef ref (Just ty)

newUniq :: (MonadIO m) => Tc m Uniq
newUniq = Tc $ \env -> do
        let ref = tc_uniq env
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

-- | Environment management
emptyEnv :: (MonadIO m) => m TcEnv
emptyEnv = do
        ref <- liftIO $ newIORef 0
        return $ TcEnv M.empty [] ref

extendVarEnv :: (Monad m) => Name -> Type -> Tc m a -> Tc m a
extendVarEnv x ty = local (\env -> env{tc_varenv = M.insert x ty (tc_varenv env)})

lookupVarEnv :: (MonadFail m) => Name -> Tc m Type
lookupVarEnv x = do
        env <- asks tc_varenv
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTc $ hsep ["Not in scope ", squotes $ pretty x]

extendTyvarEnv :: (Monad m) => [TyVar] -> Tc m a -> Tc m a
extendTyvarEnv xs = local (\env -> env{tc_tyvarEnv = reverse (map tyVarName xs) ++ tc_tyvarEnv env})

lookupTyvarEnv :: (MonadFail m) => Name -> Tc m ()
lookupTyvarEnv x = do
        env <- asks tc_tyvarEnv
        unless (x `elem` env) $ failTc $ hsep ["Not in scope ", squotes $ pretty x]