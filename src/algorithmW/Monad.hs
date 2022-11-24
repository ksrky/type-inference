{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Monad where

import Control.Monad.Reader
import Control.Monad.State.Class
import qualified Data.Map.Strict as M

import Subst
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

-- | fresh name generation
newTyVar :: Monad m => Infer m Type
newTyVar = do
        u <- get
        put (u + 1)
        return $ TyVar u

-- | Environment management
emptyEnv :: Env
emptyEnv = M.empty

extendEnv :: Monad m => Name -> Sigma -> Infer m a -> Infer m a
extendEnv x ty = local (M.insert x ty)

applyEnv :: Monad m => Subst -> Infer m a -> Infer m a
applyEnv s = local $ M.map (apply s)

lookupEnv :: MonadFail m => Name -> Infer m Sigma
lookupEnv x = do
        env <- ask
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> fail $ "Not in scope " ++ x