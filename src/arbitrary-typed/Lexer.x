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
\.                      { \_ -> TokDot }
\-\>                    { \_ -> TokArrow }
\:                      { \_ -> TokColon }
\∀                      { \_ -> TokAll }
\↑                      { \_ -> TokUp }
\↓                      { \_ -> TokDown }
\(                      { \_ -> TokLParen }
\)                      { \_ -> TokRParen }

@id                     { \s -> TokName s }

{
data Token
    = TokEq
    | TokBack
    | TokDot
    | TokArrow
    | TokColon
    | TokAll
    | TokUp
    | TokDown
    | TokLParen
    | TokRParen
    | TokName String
    deriving (Eq, Show)
}