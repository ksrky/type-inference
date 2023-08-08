{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syntax where

import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------

-- | Name
type Name = String

data InOut = In | Out

-- | Terms
data Term (a :: InOut) where
        TmLit :: Lit -> Term a
        TmVar :: Name -> Term a
        TmApp :: Term a -> Term a -> Term a
        TmAbs :: Name -> Term 'In -> Term 'In
        TmAbs' :: Name -> Sigma -> Term 'Out -> Term 'Out
        TmLet :: Name -> Term a -> Term a -> Term a
        TmCase :: Term a -> [(Pat, Term a)] -> Term a
        TmTApp :: Term 'Out -> [Type] -> Term 'Out
        TmTAbs :: [TyVar] -> Term 'Out -> Term 'Out

deriving instance Eq (Term a)
deriving instance Show (Term a)

-- | Literals
data Lit = LUnit deriving (Eq, Show)

-- Patterns
data Pat
        = PCon Name [Pat]
        | PVar Name
        | PLit Lit
        | PWild
        deriving (Eq, Show)

-- | Types
data Type
        = TyVar TyVar
        | TyCon TyCon
        | TyFun Type Type
        | TyAll [TyVar] Rho
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

-- | Clause
type Clause a = ([Pat], Term a)

-- | Bindings
data Bind a
        = ValBind Name [Clause a]
        | DatBind Name [TyVar] [(Name, Type)]
        deriving (Eq, Show)

-- | Specifications
data Spec
        = ValSpec Name Type
        | TypSpec Name
        deriving (Eq, Show)

-- | Declarations
data Decl a
        = BindDecl (Bind a)
        | SpecDecl Spec
        deriving (Eq, Show)

type Program a = [Decl a]

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

instance Pretty (Term a) where
        pretty (TmLit l) = pretty l
        pretty (TmVar n) = pretty n
        pretty t@TmApp{} = pprapp t
        pretty (TmAbs var body) = hcat [backslash, pretty var, dot, pretty body]
        pretty (TmAbs' var var_ty body) = hcat [backslash, pretty var, colon, pretty var_ty, dot <+> pretty body]
        pretty (TmLet var rhs body) = hsep ["let", pretty var, equals, pretty rhs, "in", pretty body]
        pretty (TmTApp body ty_args) = ppratom body <+> brackets (hsep (map pretty ty_args))
        pretty (TmTAbs tyvars body) = hcat ["Λ", hsep (map pretty tyvars), dot, space, pretty body]
        pretty (TmCase match alts) = hsep ["case", pretty match, "of", braces (concatWith (surround $ semi <> space) (map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts))]

pprapp :: Term a -> Doc ann
pprapp t = walk t []
    where
        walk :: Term a -> [Term a] -> Doc ann
        walk (TmApp t1 t2) ts = walk t1 (t2 : ts)
        walk t' ts = ppratom t' <+> sep (map ppratom ts)

ppratom :: Term a -> Doc ann
ppratom t@TmLit{} = pretty t
ppratom t@TmVar{} = pretty t
ppratom t = parens (pretty t)

instance Pretty Pat where
        pretty PWild = "_"
        pretty (PVar var) = pretty var
        pretty (PLit lit) = pretty lit
        pretty (PCon con pats) = hsep (pretty con : map pretty pats)

-- | Pretty types
instance Pretty TyVar where
        pretty (BoundTv n) = pretty n
        pretty (SkolemTv n u) = pretty n <> pretty u

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