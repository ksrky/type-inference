module Main where

import Control.Monad
import Prettyprinter
import Prettyprinter.Render.Text

import Lexer
import Parser
import Tc

main :: IO ()
main = forM_ tests $ \inp -> do
        (t, mty) <- parse (alexScanTokens inp)
        case mty of
                Nothing -> do
                        (t, _) <- inferType t
                        putDoc $ pretty t <> line
                Just ty -> do
                        t <- checkType t ty
                        putDoc $ pretty t <> line

tests :: [String]
tests =
        [ "\\x -> x"
        , "\\f -> \\x -> f x"
        , "\\f -> let x = () in f x"
        , -- Arbitrary-rank
          "\\x -> x :: ∀a. a -> a"
        , "\\x -> x :: (∀a. a -> a) -> ∀a. a -> a"
        , "\\x -> x :: ∀b. (∀a. a -> a) -> b -> b"
        , "\\x -> x :: (∀a. a -> a) -> ∀b. b -> b"
        , "\\f -> f () :: (∀a. a -> a) -> ()"
        , "\\f -> f () :: ∀a. (a -> a) -> ()"
        ]