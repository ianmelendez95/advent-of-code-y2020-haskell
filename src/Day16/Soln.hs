{-# LANGUAGE OverloadedStrings #-}

module Day16.Soln (module Day16.Soln, module Text.Megaparsec) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
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

soln16 :: IO Int 
soln16 = do validTicketInfo <- filterValidTickets <$> readTicketInfo
            print $ labelsForCol validTicketInfo
            return 0
-- soln16 = sum . findInvalidValues <$> readTicketInfo

---- input

readTicketInfo :: IO TicketInfo
readTicketInfo = parseTicketInfo <$> TIO.readFile inputFile

parseTicketInfo :: Text -> TicketInfo
parseTicketInfo content = case parse ticketInfo inputFile content of 
                            (Left err) -> error $ errorBundlePretty err
                            (Right res) -> res

inputFile :: FilePath 
inputFile = "src/Day16/short-input2.txt"

---- possible labels

labelsForCol :: TicketInfo -> [String]
labelsForCol (TicketInfo labelInfo values) = 
  let possible :: [[[String]]]
      possible = possibleLabels labelInfo values

      reduced :: [[String]]
      reduced = foldr labelIntersection (head possible) (tail possible)
   in map head (trace ("reduced: " ++ show reduced) reduced)
  where  
    labelIntersection :: [[String]] -> [[String]] -> [[String]]
    labelIntersection = zipWith intersect

possibleLabels :: [TicketLabel] -> [TicketValues] -> [[[String]]]
possibleLabels labels = (map . map) possible 
  where 
    possible :: Int -> [String]
    possible value = map ticketLabelLabel $ filter (`validForLabel` value) labels

---- find invalid

filterValidTickets :: TicketInfo -> TicketInfo 
filterValidTickets (TicketInfo labelInfo values) = 
  TicketInfo labelInfo $ filter valuesValid values
  where 
    valuesValid :: TicketValues -> Bool
    valuesValid = all (\val -> any (`validForLabel` val) labelInfo)

findInvalidValues :: TicketInfo -> [Int]
findInvalidValues ticketInfo = filter (not . validValue ticketInfo) (ticketValues' ticketInfo)

validValue :: TicketInfo -> Int -> Bool
validValue (TicketInfo labels _) value = any (`validForLabel` value) labels

validForLabel :: TicketLabel -> Int -> Bool
validForLabel (TicketLabel _ l1 h1 l2 h2) value
  = (value >= l1 && value <= h1) || (value >= l2 && value <= h2)

ticketValues' :: TicketInfo -> [Int]
ticketValues' (TicketInfo _ values) = concat values

---- megaparsec tutorials

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

ticketList :: Parser (String, [String])
ticketList = undefined

---- ticket parsing

-- data types

data TicketInfo = TicketInfo [TicketLabel] [TicketValues]
data TicketLabel = TicketLabel String Int Int Int Int
type TicketValues = [Int]

ticketLabelLabel :: TicketLabel -> String 
ticketLabelLabel (TicketLabel l _ _ _ _) = l

makeTicketLabel :: String -> (Int, Int) -> (Int, Int) -> TicketLabel
makeTicketLabel label (l1, h1) (l2, h2) = TicketLabel label l1 h1 l2 h2

instance Show TicketInfo where
  show (TicketInfo labels []) = unlines (map show labels)
  show (TicketInfo labels (yourTicket : nearbyTickets)) 
    = unlines (map show labels)
        ++ "\n"
        ++ "your ticket:\n"
        ++ showTicketValues yourTicket ++ "\n"
        ++ "\n"
        ++ "nearby tickets:\n"
        ++ unlines (map showTicketValues (take 3 nearbyTickets))

showTicketValues :: TicketValues -> String
showTicketValues = intercalate "," . map show

instance Show TicketLabel where
  show (TicketLabel label l1 h1 l2 h2) = label ++ ": " 
                                               ++ show l1 ++ "-" ++ show h1 ++ " "
                                               ++ show l2 ++ "-" ++ show h2

-- parsers

ticketInfo :: Parser TicketInfo
ticketInfo = TicketInfo <$> labels 
                        <*> ((:) <$> yourTicket <*> nearbyTickets)
  where 
    labels = many (fieldLine <* newline)

    yourTicket = newline *> string "your ticket:" *> newline 
                         *> ticketValues

    nearbyTickets = newline *> newline 
                            *> string "nearby tickets:" *> newline 
                            *> some (ticketValues <* (newline $> () <|> eof))

ticketValues :: Parser TicketValues
ticketValues = (:) <$> L.decimal <*> some (char ',' *> L.decimal)

fieldLine :: Parser TicketLabel
fieldLine = makeTicketLabel <$> fieldName 
                            <*> (char ':' *> space *> numberPair)
                            <*> (string " or " *> numberPair)
  where 
    numberPair :: Parser (Int, Int)
    numberPair = (,) <$> L.decimal 
                     <*> (char '-' *> L.decimal)

    fieldName :: Parser String 
    fieldName = (:) <$> letterChar <*> some (satisfy (/= ':')) 