{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------

-- | Name
type Name = String

-- | Term
data Term
        = TmLit Lit
        | TmVar Name
        | TmApp Term Term
        | TmAbs Name Term
        | TmLet Name Term Term
        deriving (Eq, Show)

data Lit = LUnit deriving (Eq, Show)

-- | Type
data Type
        = TyVar TyVar
        | TyCon TyCon
        | TyFun Tau Tau
        | TyAll [TyVar] Tau
        | TyMeta MetaTv
        deriving (Eq, Show)

type Tau = Type
type Sigma = Type

newtype TyVar = BoundTv Name deriving (Eq, Ord, Show)

data TyCon = TUnit deriving (Eq, Show)

data MetaTv = MetaTv Uniq (IORef (Maybe Tau))

type Uniq = Int

-- Class instances
instance Eq MetaTv where
        (MetaTv u1 _) == (MetaTv u2 _) = u1 == u2

instance Ord MetaTv where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

instance Show MetaTv where
        show (MetaTv u _) = "$" ++ show u

-- | Environment
type Env = M.Map Name Sigma

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty TyVar where
        pretty (BoundTv var) = pretty var

instance Pretty TyCon where
        pretty TUnit = "()"

instance Pretty Type where
        pretty (TyVar tv) = pretty tv
        pretty (TyCon tc) = pretty tc
        pretty (TyFun arg res) = hsep [pprty FunPrec arg, "->", pprty TopPrec res]
        pretty (TyAll tvs ty) = hsep ["∀" <> hsep (map pretty tvs) <> dot, pretty ty]
        pretty (TyMeta tv) = viaShow tv

data Prec = TopPrec | FunPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty TyAll{} = TopPrec
precty TyFun{} = FunPrec
precty _ = AtomPrec

pprty :: Prec -> Type -> Doc ann
pprty p ty
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty