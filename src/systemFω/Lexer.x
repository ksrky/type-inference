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
val                     { \_ -> TokVal }
type                    { \_ -> TokType } 
check                   { \_ -> TokCheck }

\=                      { \_ -> TokEq }
\\                      { \_ -> TokBack }
\.                      { \_ -> TokDot }
\-\>                    { \_ -> TokArrow }
\:                      { \_ -> TokColon }
\∀                      { \_ -> TokAll }
\(                      { \_ -> TokLParen }
\)                      { \_ -> TokRParen }
\;                      { \_ -> TokSemi }

@id                     { \s -> TokName s }

{
data Token
    = TokLet
    | TokIn
    | TokVal
    | TokType
    | TokCheck
    | TokEq
    | TokBack
    | TokDot
    | TokArrow
    | TokColon
    | TokAll
    | TokLParen
    | TokRParen
    | TokSemi
    | TokName String
    deriving (Eq, Show)
}