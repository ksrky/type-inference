{
{-# LANGUAGE DataKinds #-}
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%monad { IO } { >>= } { return }
%error { parseError }

%token

'data'			{ TokData }
'fun'			{ TokFun }
'let'			{ TokLet }
'in'			{ TokIn }
'where'			{ TokWhere }
'='			{ TokEq }
'\\'			{ TokBack }
'.'			{ TokDot }
'->'			{ TokArrow }
'::'			{ TokCC }
'∀'			{ TokAll }
'{'			{ TokLBrace }
'('			{ TokLParen }
'}'			{ TokRBrace }
')'			{ TokRParen }

Var             	{ TokName $$ }

%%

Decls   :: { [Decl] }
        : Bind ';' Decls                                { $1 : $3 }
        | Spec ';' Decls                                { $1 : $3 }
        | {- empty -}                                   { [] }

Bind    :: { Bind }
        : 'fun' Var 'where' '{' Clauses '}'             { ValBind $2 $4 }
        | 'data' Var 'where' '{' Constrs '}'            { DatBind $2 $4 }

Spec    :: { Spec }
        : 'fun' Var ':' Type                            { ValBind $2 $4 }
        | 'data' Var ':' Kind                           { DatBind $2 $4 }

Clauses :: { [Clause 'In] }
        : Clause ';' Clauses                            { $1 : $3 }
        | Clause                                        { [$1] }

Clause  :: { Clause 'In }
        : Pats Term                                     { Clause $1 $2 }

Constrs :: { [(Name, Type)] }
        : Constr ';' Constrs                            {}

Constr  :: { (Name, Type) }
        : Var ':' Type                                  { ($1, $3) }

-- * Terms
Term	:: { Term 'In }
	: '\\' Var '->' Term				{ TmAbs $2 $4 }
	| 'let' Var '=' Term 'in' Term			{ TmLet $2 $4 $6 }
	| 'case' Term 'of' Alts 			{ TmCase $2 $4 }
	| Term2						{ $1 }

Term2	:: { Term 'In }
	: Term2 Term1					{ TmApp $1 $2 }
	| Term1						{ $1 }

Term1	:: { Term 'In }
	: Lit						{ TmLit $1 }				
	| Var						{ TmVar $1 }
	| '(' Term ')'					{ $2 }

Alts    :: { [(Pat, Term 'In)] }
        : Alt ';' Alts                                  { $1 : $3 }
        | Alt                                           { [$1] }
        | {- empty -}                                   { [] }

Alt     :: { (Pat, Term 'In)}
        : Pat '->' Term                                 { ($1, $3) }

Lit	:: { Lit }
	: '(' ')'					{ LUnit }

-- * Patterns
Pats    :: { [Pat] }
        : APat Pats                                     { $1 : $2 }
        | {- empty -}                                   { [] }

Pat     :: { Pat }
        : Con Pats                                      { PCon $1 $2 }
        | Var                                           { PVar $1 }
        | '_'                                           { PWild }

APat    :: { Pat }
        : Con                                           { PCon $1 [] }
        | Var                                           { PVar $1 }
        | Wild                                          { PWild }
        | '(' Pat ')'                                   { $2 }

-- * Types
Type	:: { Type }
	: Type1 '->' Type				{ TyFun $1 $3 }
	| '∀' TyVars '.' Type				{ TyAll $2 $4 }
	| Type1						{ $1 }

Type1	:: { Type }
	: TyVar						{ TyVar $1 }
	| Con						{ TyCon $1 }
	| '(' Type ')'					{ $2 }

Con	:: { TyCon }
	: '(' ')'					{ TUnit }

TyVars  :: { [TyVar] }
	: TyVar TyVars					{ $1 : $2 }
	| TyVar						{ [$1] }

TyVar	:: { TyVar }
	: Var						{ BoundTv $1 }

{
parseError :: [Token] -> IO a
parseError [] = fail "parse error at EOF"
parseError (t : _) = fail $ "parse error at " ++ show t
}