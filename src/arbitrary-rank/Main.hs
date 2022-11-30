module Main where

import Control.Monad
import Prettyprinter
import Prettyprinter.Render.Text

import Lexer
import Parser
import Tc

main :: IO ()
main = forM_ tests $ \inp -> do
        (t, mty) <- case parse (alexScanTokens inp) of
                Left err -> fail err
                Right res -> return res
        case mty of
                Nothing -> do
                        ty <- inferType t
                        putDoc $ pretty ty <> line
                Just ty -> do
                        checkType t ty
                        putDoc $ pretty ty <> line

tests :: [String]
tests =
        [ "\\x -> x"
        , "\\f -> \\x -> f x"
        , "\\f -> let x = ()in f x"
        , -- Arbitrary-rank
          "\\x -> x :: ∀a. a -> a"
        , "\\x -> x :: (∀a. a -> a) -> ∀a. a -> a"
        , "\\x -> x :: ∀b. (∀a. a -> a) -> b -> b"
        , "\\f -> f () :: (∀a. a -> a) -> ()"
        , "\\f -> f () :: ∀a. (a -> a) -> ()"
        ]