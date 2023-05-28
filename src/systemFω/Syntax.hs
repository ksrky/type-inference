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
        | TmAbs Name (Maybe Sigma) Term
        | TmPAbs Pat (Maybe Sigma) Term
        | TmLet Name Term Term
        | TmTApp Term [Type]
        | TmTAbs [TyVar] Term
        deriving (Eq, Show)

data Lit
        = LUnit
        | LPair [Term]
        | LTag Name Term
        deriving (Eq, Show)

-- | Type
data Type
        = TyVar TyVar
        | TyCon TyCon
        | TyFun Type Type
        | TyAll [TyVar] Sigma
        | TyApp Type Type
        | TyAbs Name Type
        | TyMeta MetaTv
        deriving (Eq, Show)

type Tau = Type
type Rho = Type
type Sigma = Type

data TyVar
        = BoundTv Name
        | SkolemTv Name Uniq
        deriving (Eq, Ord, Show)

data TyCon
        = TUnit
        | TPair [Tau]
        | TSum [Type]
        deriving (Eq, Show)

data MetaTv = MetaTv Uniq (IORef (Maybe Tau))

type Uniq = Int

tyVarName :: TyVar -> Name
tyVarName (BoundTv n) = n
tyVarName (SkolemTv n _) = n

instance Eq MetaTv where
        (MetaTv u1 _) == (MetaTv u2 _) = u1 == u2

instance Ord MetaTv where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

instance Show MetaTv where
        show (MetaTv u _) = "$" ++ show u

-- Pattern
data Pat
        = PVar Name
        | PWild
        | PCon Name [Pat]
        deriving (Eq, Show)

-- Command
data Command
        = TmBind Name Type Term
        | TyBind Name Type
        | TyCheck Term Type

-- | Environment
type Env = M.Map Name Sigma

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------

-- | Pretty terms
instance Pretty Lit where
        pretty (LPair fields) = "()"
        pretty (LTag lab arg) = pretty lab <+> pretty arg

instance Pretty Term where
        pretty (TmLit l) = pretty l
        pretty (TmVar n) = pretty n
        pretty t@TmApp{} = pprapp t
        pretty (TmAbs var Nothing body) = hcat [backslash, pretty var, dot, space, pretty body]
        pretty (TmAbs var (Just var_ty) body) = hcat [backslash, pretty var, colon, pretty var_ty, dot, space, pretty body]
        pretty (TmPAbs pat Nothing body) = hcat [backslash, pretty pat, dot, space, pretty body]
        pretty (TmPAbs pat (Just pat_ty) body) = hcat [backslash, pretty pat, colon, pretty pat_ty, dot, space, pretty body]
        pretty (TmLet var rhs body) = hsep ["let", pretty var, equals, pretty rhs, "in", pretty body]
        pretty (TmTApp body ty_args) = ppratom body <+> hsep (map (\x -> "@" <> pretty x) ty_args)
        pretty (TmTAbs ty_vars body) = "Λ" <> hsep (map (\x -> pretty x <> dot) ty_vars) <+> pretty body

pprapp :: Term -> Doc ann
pprapp t = walk t []
    where
        walk :: Term -> [Term] -> Doc ann
        walk (TmApp t1 t2) ts = walk t1 (t2 : ts)
        walk t' ts = ppratom t' <+> sep (map ppratom ts)

ppratom :: Term -> Doc ann
ppratom t@TmLit{} = pretty t
ppratom t@TmVar{} = pretty t
ppratom t = parens (pretty t)

-- | Pretty types
instance Pretty TyVar where
        pretty (BoundTv n) = pretty n
        pretty (SkolemTv n u) = pretty n <> pretty u

instance Pretty TyCon where
        pretty (TPair fields) = "()"

instance Pretty Type where
        pretty (TyVar tv) = pretty tv
        pretty (TyCon tc) = pretty tc
        pretty (TyFun arg res) = hsep [pprty FunPrec arg, "->", pprty TopPrec res]
        pretty (TyAll tvs body) = hcat ["∀", hsep (map pretty tvs), dot, space, pretty body]
        pretty (TyApp fun arg) = pretty fun <+> pprty AppPrec arg
        pretty (TyAbs var body) = hcat [backslash, pretty var, dot, space, pretty body]
        pretty (TyMeta tv) = viaShow tv

data Prec = TopPrec | FunPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty TyAll{} = TopPrec
precty TyFun{} = FunPrec
precty TyApp{} = AppPrec
precty _ = AtomPrec

pprty :: Prec -> Type -> Doc ann
pprty p ty
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty

instance Pretty Pat where
        pretty (PVar var) = pretty var
        pretty PWild = "_"
        pretty (PCon con []) = pretty con
        pretty (PCon con pats) = parens $ hcat [lparen, pretty con, space, hsep (map pretty pats)]