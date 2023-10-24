{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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

-- | Terms
data Term
        = TmLit Lit
        | TmVar Name
        | TmApp Term Poly
        | TmAbs Name (Maybe Sigma) Poly
        | TmInst Poly
        deriving (Eq, Show)

-- | Polymorphic terms
data Poly
        = TmTApp Poly [Tau]
        | TmTAbs [TyVar] Poly
        | TmGen Term
        deriving (Eq, Show)

-- | Literals
data Lit = LUnit deriving (Eq, Show)

-- | Types
data Type
        = TyVar TyVar
        | TyCon TyCon
        | TyFun Type Type
        | TyAll [TyVar] Type
        | TyMeta MetaTv
        deriving (Eq, Show)

type Tau = Type
type Rho = Type
type Sigma = Type

data TyVar
        = BoundTv Name
        | SkolemTv Name Uniq
        deriving (Eq, Ord, Show)

tyVarName :: TyVar -> Name
tyVarName (BoundTv n) = n
tyVarName (SkolemTv n _) = n

data TyCon = TUnit deriving (Eq, Show)

data MetaTv = MetaTv Uniq (IORef (Maybe Tau))

type Uniq = Int

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

-- | Pretty terms
instance Pretty Lit where
        pretty LUnit = "()"

instance Pretty Term where
        pretty (TmLit l) = pretty l
        pretty (TmVar n) = pretty n
        pretty t@TmApp{} = pprapp t
        pretty (TmAbs var _ body) = hcat [backslash, pretty var, dot, pretty body]
        pretty (TmInst poly) = hcat ["↓", pretty poly]

instance Pretty Poly where
        pretty (TmTApp body ty_args) = ppratomPoly body <+> brackets (hsep (map (pprtyatom TopPrec) ty_args))
        pretty (TmTAbs tyvars body) = hcat ["/\\", hsep (map pretty tyvars), dot, space, pretty body]
        pretty (TmGen term) = hcat ["↑", pretty term]

pprapp :: Term -> Doc ann
pprapp t = walk t []
    where
        walk :: Term -> [Poly] -> Doc ann
        walk (TmApp t1 t2) ts = walk t1 (t2 : ts)
        walk t' ts = ppratom t' <+> sep (map ppratomPoly ts)

ppratom :: Term -> Doc ann
ppratom t@TmLit{} = pretty t
ppratom t@TmVar{} = pretty t
ppratom t = parens (pretty t)

ppratomPoly :: Poly -> Doc ann
ppratomPoly t@TmTApp{} = parens (pretty t)
ppratomPoly t@TmTAbs{} = parens (pretty t)
ppratomPoly t = pretty t

-- | Pretty types
instance Pretty TyVar where
        pretty (BoundTv n) = pretty n
        pretty (SkolemTv n u) = pretty n <> pretty u

instance Pretty TyCon where
        pretty TUnit = "()"

instance Pretty Type where
        pretty (TyVar tv) = pretty tv
        pretty (TyCon tc) = pretty tc
        pretty (TyFun arg res) = hsep [pprtyatom FunPrec arg, "->", pprtyatom TopPrec res]
        pretty (TyAll tvs ty) = hsep ["∀" <> hsep (map pretty tvs) <> dot, pretty ty]
        pretty (TyMeta tv) = viaShow tv

data Prec = TopPrec | FunPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty TyAll{} = TopPrec
precty TyFun{} = FunPrec
precty _ = AtomPrec

pprtyatom :: Prec -> Type -> Doc ann
pprtyatom p ty
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty