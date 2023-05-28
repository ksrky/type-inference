{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%monad { IO } { >>= } { return }
%error { parseError }

%token

'let'			{ TokLet }
'in'			{ TokIn }
'val'			{ TokVal }
'type'			{ TokType }
'check'			{ TokCheck }
'='			{ TokEq }
'\\'			{ TokBack }
'.'			{ TokDot }
'->'			{ TokArrow }
':'			{ TokColon }
'∀'			{ TokAll }
'('			{ TokLParen }
')'			{ TokRParen }
';'			{ TokSemi }

Var             	{ TokName $$ }

%%

Dummy	:: { Command }
	: {- empty -}					{ undefined }

{- 
Decl	:: { Command }
	: 'val' Var ':' Type '=' Term ';'		{ TmBind $2 $4 $6 }	
	| 'type' Var '=' Type ';'			{ TyBind $2 $4 }
	| 'check' Term ':' Type ';'			{ TyCheck $2 $4 }

Term	:: { Term }
	: '\\' Var '->' Term				{ TmAbs $2 Nothing $4 }
	| 'let' Var '=' Term 'in' Term			{ TmLet $2 $4 $6 }
	| Term2						{ $1 }

Term2	:: { Term }
	: Term2 Term1					{ TmApp $1 $2 }
	| Term1						{ $1 }

Term1	:: { Term }
	: Lit						{ TmLit $1 }				
	| Var						{ TmVar $1 }
	| '(' Term ')'					{ $2 }

Lit	:: { Lit }
	: '(' ')'					{ LUnit }

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
-}
{
parseError :: [Token] -> IO a
parseError [] = fail "parse error at EOF"
parseError (t : _) = fail $ "parse error at " ++ show t
}