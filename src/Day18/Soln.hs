{-# LANGUAGE OverloadedStrings #-}

module Day18.Soln where 

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List

import Data.Functor

import Data.Char

import Debug.Trace

type Parser = Parsec Void Text

---- soln

soln18 :: IO Int 
soln18 = sum . map reduce <$> parseExpressions 

---- input

parseExpressions :: IO [Expr]
parseExpressions = parseText <$> TIO.readFile inputFile
  where 
    parseText :: Text -> [Expr]
    parseText content = case parse pExpressions inputFile content of 
                          (Left err) -> error $ errorBundlePretty err
                          (Right res) -> res

inputFile :: FilePath
inputFile = "src/Day18/full-input.txt"

---- expressions

data Expr 
  = Int Int 
  | Sum Expr Expr
  | Product Expr Expr
  deriving (Eq, Ord, Show)

-- reduction

reduce :: Expr -> Int
reduce (Int x) = x
reduce (Sum e e') = reduce e + reduce e'
reduce (Product e e') = reduce e * reduce e'

-- parsing expressions: https://markkarpov.com/tutorial/megaparsec.html#parsing-expressions

testParser1 :: IO ()
testParser1 = parseTest (pExpr <* eof) "1 * (2 + 3)"

testParser2 :: IO ()
testParser2 = parseTest (pExpr <* eof) "1 + 2 * 3 + 4 * 5 + 6"

pExpressions :: Parser [Expr]
pExpressions = some pExpr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = 
  [[ binary "+" Sum ], [binary "*" Product]]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

pTerm :: Parser Expr
pTerm = choice [parens pExpr, pInteger]

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal 

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc