{-# LANGUAGE TupleSections #-}

module Parser where

import Syntax

import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc =
        L.space
                space1
                (L.skipLineComment "//")
                (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pName :: Parser String
pName = (:) <$> letterChar <*> many alphaNumChar <?> "`Name`"

parens :: Parser a -> Parser a
parens = between (symbol "(") (string ")")

pTerm :: Parser Term
pTerm =
        makeExprParser
                ( choice
                        [ try pTmLet
                        , try pTmLit
                        , pTmAbs
                        , parens $ lexeme pTerm
                        , pTmVar
                        ]
                )
                [[InfixL $ TmApp <$ symbol " "]]
                <?> "`Term`"

pTmLit :: Parser Term
pTmLit = TmLit LUnit <$ string "*"

pTmVar :: Parser Term
pTmVar = TmVar <$> pName

pTmAbs :: Parser Term
pTmAbs = TmAbs <$> between (symbol "\\") (symbol "->") (lexeme pName) <*> pTerm

pTmLet :: Parser Term
pTmLet = do
        _ <- symbol "let"
        var <- lexeme pName
        _ <- symbol "="
        rhs <- lexeme pTerm
        _ <- symbol "in"
        TmLet var rhs <$> pTerm

pTy :: Parser Type
pTy =
        makeExprParser
                ( choice
                        [ try pTyAll
                        , try pTyCon
                        , parens $ lexeme pTy
                        , pTyVar
                        ]
                )
                [[InfixR (TyFun <$ symbol "->")]]
                <?> "`Type`"

pTyCon :: Parser Type
pTyCon = TyCon TUnit <$ string "()"

pTyVar :: Parser Type
pTyVar = TyVar . BoundTv <$> pName

pTyAll :: Parser Type
pTyAll = do
        _ <- symbol "∀"
        tvs <- many $ BoundTv <$> lexeme pName
        _ <- symbol "."
        TyAll tvs <$> pTy

pExpr :: Parser (Term, Maybe Type)
pExpr =
        try ((,) <$> (pTerm <* symbol "::") <*> (Just <$> pTy))
                <|> (,Nothing) <$> pTerm

parseLine :: MonadFail m => String -> m (Term, Maybe Type)
parseLine inp = case parse (pExpr <* eof) "" inp of
        Left err -> fail $ errorBundlePretty err
        Right res -> return res