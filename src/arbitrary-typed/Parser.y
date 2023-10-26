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

'='			{ TokEq }
'\\'			{ TokBack }
'/\\'                   { TokBiglam }
'.'			{ TokDot }
'->'			{ TokArrow }
':'			{ TokColon }
'('			{ TokLParen }
')'			{ TokRParen }
'['			{ TokLBrack }
']'			{ TokRBrack }
'{'			{ TokLBrace }
'}'			{ TokRBrace }

Var             	{ TokName $$ }

%%

Expr 	:: { (Term, Maybe Type) }
	: Term ':' Type 				{ ($1, Just $3) }
	| Term						{ ($1, Nothing) }

Term	:: { Term }
        : '\\' '(' Var ':' Type ')' '.' Term		{ TmAbs $3 (Just $5) $8 }
	| '\\' Var '.' Term				{ TmAbs $2 Nothing $4 }
        | '/\\' TyVars '.' Term			        { TmTAbs $2 $4 }
	| Term2						{ $1 }

Term2	:: { Term }
	: Term2 Term1					{ TmApp $1 $2 }
        | Term2 '[' Types ']'                           { TmTApp $1 $3 }
	| Term1						{ $1 }

Term1	:: { Term }
	: Lit						{ TmLit $1 }				
	| Var						{ TmVar $1 }
	| '(' Term ')'					{ $2 }

Lit	:: { Lit }
	: '(' ')'					{ LUnit }

Type	:: { Type }
	: Type1 '->' Type				{ TyFun $1 $3 }
	| '{' TyVars '}' Type				{ TyAll $2 $4 }
	| Type1						{ $1 }

Type1	:: { Type }
	: TyVar						{ TyVar $1 }
	| Con						{ TyCon $1 }
	| '(' Type ')'					{ $2 }

Types   :: { [Type] }
        : Type1 Types					{ $1 : $2 }
        | Type1						{ [$1] }

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