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

-- | Inference monad
newtype Infer m a = Infer {runInfer :: Env -> Uniq -> m (a, Uniq)}

instance Monad m => Functor (Infer m) where
        fmap f (Infer m) = Infer $ \env uniq -> do
                (x, uniq') <- m env uniq
                return (f x, uniq')

instance Monad m => Applicative (Infer m) where
        pure x = Infer (\_ uniq -> return (x, uniq))
        f <*> x = Infer $ \env uniq -> do
                (f', uniq') <- runInfer f env uniq
                runInfer (fmap f' x) env uniq'

instance Monad m => Monad (Infer m) where
        m >>= k = Infer $ \env uniq -> do
                (v, uniq') <- runInfer m env uniq
                runInfer (k v) env uniq'

instance MonadFail m => MonadFail (Infer m) where
        fail s = Infer $ \_ _ -> fail s

instance MonadTrans Infer where
        lift m = Infer (\_ uniq -> (,uniq) <$> m)

instance Monad m => MonadState Uniq (Infer m) where
        get = Infer (\_ uniq -> return (uniq, uniq))
        put uniq = Infer (\_ _ -> return ((), uniq))

instance Monad m => MonadReader Env (Infer m) where
        ask = Infer (curry return)
        local f (Infer m) = Infer (m . f)

failInf :: MonadFail m => Doc ann -> Infer m a
failInf doc = fail $ renderString $ layoutPretty defaultLayoutOptions doc

-- | IORef
newInfRef :: MonadIO m => a -> Infer m (IORef a)
newInfRef v = lift (liftIO $ newIORef v)

readInfRef :: MonadIO m => IORef a -> Infer m a
readInfRef r = lift (liftIO $ readIORef r)

writeInfRef :: MonadIO m => IORef a -> a -> Infer m ()
writeInfRef r v = lift (liftIO $ writeIORef r v)

-- | fresh name generation
newTyVar :: MonadIO m => Infer m Type
newTyVar = TyMeta <$> newMetaTv

newMetaTv :: MonadIO m => Infer m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newInfRef Nothing

readMetaTv :: MonadIO m => MetaTv -> Infer m (Maybe Tau)
readMetaTv (MetaTv _ ref) = readInfRef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> Infer m ()
writeMetaTv (MetaTv _ ref) ty = writeInfRef ref (Just ty)

newUniq :: Monad m => Infer m Uniq
newUniq = do
        u <- get
        put (u + 1)
        return u

-- | Environment management
emptyEnv :: Env
emptyEnv = M.empty

extendEnv :: Monad m => Name -> Sigma -> Infer m a -> Infer m a
extendEnv x ty = local (M.insert x ty)

lookupEnv :: MonadFail m => Name -> Infer m Sigma
lookupEnv x = do
        env <- ask
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failInf $ hsep ["Not in scope ", squotes $ pretty x]