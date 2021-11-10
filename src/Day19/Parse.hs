{-# LANGUAGE OverloadedStrings #-}

module Day19.Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Pos 
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor

import Data.List

type Parser = Parsec Void T.Text


type FullSpec = ([Decl], [Message])

type Decl = (Int, Rule)

type Message = T.Text

data Rule = RExact  Char
          | RAltSeq [[Int]]

instance Show Rule where 
  show (RExact c) = "\"" ++ [c] ++ "\""
  show (RAltSeq seqs) = intercalate " | " (map (unwords . map show) seqs)


--------------------------------------------------------------------------------
-- Spec

parseFullSpec :: FilePath -> IO FullSpec
parseFullSpec file = 
  do input <- TIO.readFile file
     case parse fullSpec file input of
       Left bundle -> error (errorBundlePretty bundle)
       Right spec -> pure spec

fullSpec :: Parser FullSpec
fullSpec = 
  do decls <- some (declaration <* newline)
     _ <- newline
     msgs  <- some (message <* (newline $> () <|> eof))
     pure (decls, msgs)

showFullSpec :: FullSpec -> String
showFullSpec (decls, msgs) = unlines (map showDecl decls ++ [""] ++ map T.unpack msgs)
  where 
    showDecl :: Decl -> String
    showDecl (idx, rule) = show idx ++ ": " ++ show rule


--------------------------------------------------------------------------------
-- Message

message :: Parser T.Text
message = T.pack <$> some letterChar


--------------------------------------------------------------------------------
-- Declaration


declaration :: Parser Decl
declaration = 
  (,) <$> lexeme (L.decimal <* symbol ":")
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