{-# LANGUAGE OverloadedStrings #-}

module Day19.Soln where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Pos 
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import qualified Data.Text as T
import Data.Functor


type Parser = Parsec Void T.Text


data Decl = Decl {
  declIndex :: Int,
  declRule  :: Rule
} deriving Show

data Rule = RExact  Char
          | RAltSeq [[Int]]
          deriving Show


--------------------------------------------------------------------------------
-- Declaration

parseTestDecl :: String -> IO ()
parseTestDecl = parseTest decl . T.pack

decl :: Parser Decl
decl = 
  Decl <$> lexeme (L.decimal <* symbol ":")
       <*> rule


--------------------------------------------------------------------------------
-- Rule
     
rule :: Parser Rule
rule = 
  choice [ ruleExact 
         , ruleAltSeq 
         ]

ruleExact :: Parser Rule
ruleExact = RExact <$> (char '"' *> letterChar <* char '"')

ruleAltSeq :: Parser Rule
ruleAltSeq = RAltSeq <$> sepBy1 ruleSeq (symbol "|")
  where 
    ruleSeq :: Parser [Int]
    ruleSeq = some (lexeme L.decimal)


--------------------------------------------------------------------------------
-- Space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme solnSpace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol solnSpace

solnSpace :: Parser ()
solnSpace = L.space (char ' ' $> ()) empty empty
