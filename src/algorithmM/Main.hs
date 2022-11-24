module Main where

import Control.Monad

import Infer
import Syntax

main :: IO ()
main = forM_ tests $ \t -> do
        ty <- inferType t
        print ty

tests :: [Term]
tests =
        [ TmAbs "x" (TmVar "x") -- \x -> x
        , TmAbs "f" (TmAbs "x" (TmApp (TmVar "f") (TmVar "x"))) -- \f x -> f x
        , TmAbs "f" (TmLet "x" (TmLit (IntL 1)) (TmApp (TmVar "f") (TmVar "x"))) -- \f -> let x = 1 in f x
        , TmAbs "x" (TmApp (TmLit (IntL 3)) (TmVar "x")) -- \x -> 3 x
        ]