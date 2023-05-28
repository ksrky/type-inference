{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Monad.IO.Class
import Prettyprinter
import Prettyprinter.Render.Text
import System.Console.Haskeline

import Lexer
import Parser
import Tc

main :: IO ()
main = repl

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> loop
                        Just ":quit" -> return ()
                        Just ":test" -> liftIO (mapM_ process tests) >> loop
                        Just input -> liftIO (process input) >> loop

process :: String -> IO ()
process inp =
        catch
                ( do
                        (t, mty) <- parse (alexScanTokens inp)
                        case mty of
                                Nothing -> do
                                        (t, _) <- inferType t
                                        putDoc $ pretty t <> line
                                Just ty -> do
                                        t <- checkType t ty
                                        putDoc $ pretty t <> line
                )
                (\(e :: SomeException) -> print e)

tests :: [String]
tests =
        [ "\\x -> x"
        , "\\f -> \\x -> f x"
        , "\\f -> let x = () in f x"
        , -- Arbitrary-rank
          "\\x -> x :: ∀a. a -> a"
        , "\\f -> \\x -> f x :: ∀a b. (a -> b) -> a -> b"
        , "\\f -> \\x -> f x :: ∀a. ∀b. (a -> b) -> a -> b"
        , "\\x -> x :: (∀a. a -> a) -> ∀a. a -> a"
        , "\\x -> x :: ∀b. (∀a. a -> a) -> b -> b"
        , "\\x -> x :: (∀a. a -> a) -> ∀b. b -> b"
        , "\\f -> f () :: (∀a. a -> a) -> ()"
        , "\\f -> f () :: ∀a. (a -> a) -> ()"
        ]