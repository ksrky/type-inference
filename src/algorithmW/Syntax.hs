module Syntax where

import qualified Data.Map.Strict as M

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
        = TyVar Uniq
        | TyCon TyCon
        | TyFun Tau Tau
        | TyAll [Uniq] Sigma
        deriving (Eq, Show)

type Tau = Type
type Sigma = Type

type Uniq = Int

data TyCon = TUnit deriving (Eq, Show)

-- | Environment
type Env = M.Map Name Sigma