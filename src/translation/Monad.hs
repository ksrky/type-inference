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
        { tc_env :: Env
        , tc_uniq :: IORef Uniq
        }

-- | Tc monad
newtype Tc m a = Tc {runTc :: TcEnv -> m a}

instance Monad m => Functor (Tc m) where
        fmap f (Tc m) = Tc $ \env -> do
                x <- m env
                return (f x)

instance Monad m => Applicative (Tc m) where
        pure x = Tc (\_ -> return x)
        f <*> x = Tc $ \env -> do
                f' <- runTc f env
                runTc (fmap f' x) env

instance Monad m => Monad (Tc m) where
        m >>= k = Tc $ \env -> do
                v <- runTc m env
                runTc (k v) env

instance MonadFail m => MonadFail (Tc m) where
        fail s = Tc $ \_ -> fail s

instance MonadTrans Tc where
        lift m = Tc (const m)

instance Monad m => MonadReader Env (Tc m) where
        ask = Tc (return . tc_env)
        local f (Tc m) = Tc (\env -> m env{tc_env = f (tc_env env)})

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

newUniq :: MonadIO m => Tc m Uniq
newUniq = Tc $ \env -> do
        let ref = tc_uniq env
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

-- | Environment management
emptyEnv :: MonadIO m => m TcEnv
emptyEnv = do
        ref <- liftIO $ newIORef 0
        return $ TcEnv M.empty ref

extendEnv :: Monad m => Name -> Sigma -> Tc m a -> Tc m a
extendEnv x ty = local (M.insert x ty)

lookupEnv :: MonadFail m => Name -> Tc m Sigma
lookupEnv x = do
        env <- ask
        case M.lookup x env of
                Just ty -> return ty
                Nothing -> failTc $ hsep ["Not in scope ", squotes $ pretty x]