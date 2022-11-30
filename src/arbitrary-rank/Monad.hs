{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Monad where

import Control.Monad.Reader
import Control.Monad.State.Class
import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter
import Prettyprinter.Render.String

import Syntax

-- | Tc monad
newtype Tc m a = Tc {runTc :: Env -> Uniq -> m (a, Uniq)}

instance Monad m => Functor (Tc m) where
        fmap f (Tc m) = Tc $ \env uniq -> do
                (x, uniq') <- m env uniq
                return (f x, uniq')

instance Monad m => Applicative (Tc m) where
        pure x = Tc (\_ uniq -> return (x, uniq))
        f <*> x = Tc $ \env uniq -> do
                (f', uniq') <- runTc f env uniq
                runTc (fmap f' x) env uniq'

instance Monad m => Monad (Tc m) where
        m >>= k = Tc $ \env uniq -> do
                (v, uniq') <- runTc m env uniq
                runTc (k v) env uniq'

instance MonadFail m => MonadFail (Tc m) where
        fail s = Tc $ \_ _ -> fail s

instance MonadTrans Tc where
        lift m = Tc (\_ uniq -> (,uniq) <$> m)

instance Monad m => MonadState Uniq (Tc m) where
        get = Tc (\_ uniq -> return (uniq, uniq))
        put uniq = Tc (\_ _ -> return ((), uniq))

instance Monad m => MonadReader Env (Tc m) where
        ask = Tc (curry return)
        local f (Tc m) = Tc (m . f)

failTc :: MonadFail m => Doc ann -> Tc m a
failTc doc = fail $ renderString $ layoutPretty defaultLayoutOptions doc

-- | IORef
newTcRef :: MonadIO m => a -> Tc m (IORef a)
newTcRef v = lift (liftIO $ newIORef v)

readTcRef :: MonadIO m => IORef a -> Tc m a
readTcRef r = lift (liftIO $ readIORef r)

writeTcRef :: MonadIO m => IORef a -> a -> Tc m ()
writeTcRef r v = lift (liftIO $ writeIORef r v)

-- | type variable generation
newTyVar :: MonadIO m => Tc m Type
newTyVar = TyMeta <$> newMetaTv

newSkolemTyVar :: MonadIO m => TyVar -> Tc m TyVar
newSkolemTyVar tv = SkolemTv (tyVarName tv) <$> newUniq

newMetaTv :: MonadIO m => Tc m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newTcRef Nothing

readMetaTv :: MonadIO m => MetaTv -> Tc m (Maybe Tau)
readMetaTv (MetaTv _ ref) = readTcRef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> Tc m ()
writeMetaTv (MetaTv _ ref) ty = writeTcRef ref (Just ty)

newUniq :: Monad m => Tc m Uniq
newUniq = do
        u <- get
        put (u + 1)
        return u

-- | Environment management
emptyEnv :: Env
emptyEnv = M.empty

extendEnv :: Monad m => Name -> Sigma -> Tc m a -> Tc m a
extendEnv x ty = local (M.insert x ty)

lookupEnv :: MonadFail m => Name -> Tc m Sigma
lookupEnv x = do
        env <- ask
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTc $ hsep ["Not in scope ", squotes $ pretty x]