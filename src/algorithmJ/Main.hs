module Main where

import Control.Monad
import Prettyprinter
import Prettyprinter.Render.Text

import Infer
import Syntax

main :: IO ()
main = forM_ tests $ \t -> do
        ty <- inferType t
        putDoc $ pretty ty <> line

tests :: [Term]
tests =
        [ TmAbs "x" (TmVar "x") -- \x -> x
        , TmAbs "f" (TmAbs "x" (TmApp (TmVar "f") (TmVar "x"))) -- \f x -> f x
        , TmAbs "f" (TmLet "x" (TmLit LUnit) (TmApp (TmVar "f") (TmVar "x"))) -- \f -> let x = 1 in f x
        , TmAbs "x" (TmApp (TmLit LUnit) (TmVar "x")) -- \x -> 3 x
        ]