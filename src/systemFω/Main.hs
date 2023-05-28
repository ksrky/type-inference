module Main where

import Control.Monad
import Control.Monad.IO.Class
import Prettyprinter
import Prettyprinter.Render.Text

import Lexer
import Monad
import Parser
import Syntax
import Tc

main :: IO ()
main = undefined {- forM_ tests $ \inp -> do
                         cmds <- parse (alexScanTokens inp)
                         case mty of
                                 Nothing -> do
                                         (t, _) <- inferType t
                                         putDoc $ pretty t <> line
                                 Just ty -> do
                                         t <- checkType t ty
                                         putDoc $ pretty t <> line

                 process :: (MonadFail m, MonadIO m) => [Command] -> Tc m ()
                 process (TmBind x ty t : cmds) = extendEnv x ty $ process cmds
                 process (TyBind x ty : cmds) = undefined
                 process (TyCheck t ty : cmds) = do
                         t' <- checkSigma t ty
                         liftIO $ putDoc $ pretty t' <> line-}