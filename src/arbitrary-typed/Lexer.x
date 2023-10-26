{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@id = $alpha [$alpha $digit \_ \']*

tokens :-

$white+                 ;

\=                      { \_ -> TokEq }
\\                      { \_ -> TokBack }
\/\\                    { \_ -> TokBiglam }
\.                      { \_ -> TokDot }
\-\>                    { \_ -> TokArrow }
\:                      { \_ -> TokColon }
\∀                      { \_ -> TokAll }
\(                      { \_ -> TokLParen }
\)                      { \_ -> TokRParen }
\[                      { \_ -> TokLBrack }
\]                      { \_ -> TokRBrack }
\{                      { \_ -> TokLBrace }
\}                      { \_ -> TokRBrace }

@id                     { \s -> TokName s }

{
data Token
    = TokEq
    | TokBack
    | TokBiglam
    | TokDot
    | TokArrow
    | TokColon
    | TokAll
    | TokLParen
    | TokRParen
    | TokLBrack
    | TokRBrack
    | TokLBrace
    | TokRBrace
    | TokName String
    deriving (Eq, Show)
}