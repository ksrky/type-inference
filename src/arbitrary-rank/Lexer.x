{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@id = $alpha [$alpha $digit \_ \']*

tokens :-

$white+                 ;
let                     { \_ -> TokLet }
in                      { \_ -> TokIn }

\=                      { \_ -> TokEq }
\\						{ \_ -> TokBack }
\.						{ \_ -> TokDot }
\-\>					{ \_ -> TokArrow }
\:\:					{ \_ -> TokCC }
\∀						{ \_ -> TokAll }
\(						{ \_ -> TokLParen }
\)						{ \_ -> TokRParen }

@id                     { \s -> TokName s }

{
data Token
    = TokLet
	| TokIn
	| TokEq
	| TokBack
	| TokDot
	| TokArrow
	| TokCC
	| TokAll
	| TokLParen
	| TokRParen
	| TokName String
	deriving (Eq, Show)
}