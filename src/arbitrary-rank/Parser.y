{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%monad { Error } { >>= } { return }
%error { parseError }

%token

'let'			{ TokLet }
'in'			{ TokIn }
'='				{ TokEq }
'\\'			{ TokBack }
'.'				{ TokDot }
'->'			{ TokArrow }
'::'			{ TokCC }
'∀'				{ TokAll }
'('				{ TokLParen }
')'				{ TokRParen }

Var             { TokName $$ }

%%

Expr 	:: { (Term, Maybe Type) }
		: Term '::' Type					{ ($1, Just $3) }
		| Term								{ ($1, Nothing) }

Term	:: { Term }
		: '\\' Var '->' Term				{ TmAbs $2 $4 }
		| 'let' Var '=' Term 'in' Term		{ TmLet $2 $4 $6 }
		| Term2								{ $1 }

Term2	:: { Term }
		: Term2 Term1						{ TmApp $1 $2 }
		| Term1								{ $1 }

Term1	:: { Term }
		: Lit								{ TmLit $1 }				
		| Var								{ TmVar $1 }
		| '(' Term ')'						{ $2 }

Lit		:: { Lit }
		: '(' ')'							{ LUnit }

Type	:: { Type }
		: Type1 '->' Type					{ TyFun $1 $3 }
		| '∀' TyVars '.' Type				{ TyAll $2 $4 }
		| Type1								{ $1 }

Type1	:: { Type }
		: TyVar								{ TyVar $1 }
		| Con								{ TyCon $1 }
		| '(' Type ')'						{ $2 }

Con		:: { TyCon }
		: '(' ')'							{ TUnit }

TyVars  :: { [TyVar] }
		: TyVar TyVars						{ $1 : $2 }
		| TyVar								{ [$1] }

TyVar	:: { TyVar }
		: Var								{ BoundTv $1 }

{
type Error = Either String

parseError :: [Token] -> Error a
parseError ts = Left $ "Parse error: " ++ show ts
}