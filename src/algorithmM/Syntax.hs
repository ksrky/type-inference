module Syntax where

import qualified Data.Map.Strict as M

-- | Name
type Name = String

-- | Term
data Term
        = TmLit TmLit
        | TmVar Name
        | TmApp Term Term
        | TmAbs Name Term
        | TmLet Name Term Term
        deriving (Eq, Show)

newtype TmLit = IntL Int deriving (Eq, Show)

-- | Type
data Type
        = TyVar Uniq
        | TyCon TyCon
        | TyFun Tau Tau
        | TyAll [Uniq] Tau
        deriving (Eq, Show)

type Tau = Type
type Sigma = Type

type Uniq = Int

data TyCon = IntT deriving (Eq, Show)

-- | Environment
type Env = M.Map Name Sigma