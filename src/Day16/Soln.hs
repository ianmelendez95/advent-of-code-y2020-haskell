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
soln16 = departureProduct <$> readTicketInfo

---- input

readTicketInfo :: IO TicketInfo
readTicketInfo = parseTicketInfo <$> TIO.readFile inputFile

parseTicketInfo :: Text -> TicketInfo
parseTicketInfo content = case parse ticketInfo inputFile content of 
                            (Left err) -> error $ errorBundlePretty err
                            (Right res) -> res

inputFile :: FilePath 
inputFile = "src/Day16/full-input.txt"

---- validate labels

validateLabels :: TicketInfo -> [String] -> Bool
validateLabels ticketInfo labelNames = all ticketValuesValid (ticketInfoValues ticketInfo)
  where 
    ticketValuesValid :: TicketValues -> Bool
    ticketValuesValid values = and (zipWith validForLabel labelsFromNames values) 

    labelsFromNames :: [TicketLabel]
    labelsFromNames = map (\name -> head $ filter ((==name) . ticketLabelLabel) labels) labelNames

    labels = ticketInfoLabels ticketInfo

---- sum departure fields 

departureProduct :: TicketInfo -> Int 
departureProduct ticketInfo = let labels = identifyLabels (possibleLabels (filterValidTickets ticketInfo))
                                  labelledTicket = zip labels (myTicketValues ticketInfo)
                               in product . map snd . filter (("departure" `isPrefixOf`) . fst) $ labelledTicket

---- identify labels from possible

identifyLabels :: [[String]] -> [String]
identifyLabels labels = let pass = map reduceLabels labels
                         in if labelsComplete pass then map head pass else identifyLabels pass
  where 
    reduceLabels :: [String] -> [String]
    reduceLabels labels = case length labels of 
                            0 -> error "Empty labels"
                            1 -> labels 
                            _ -> labels \\ singletonSet

    labelsComplete :: [[String]] -> Bool
    labelsComplete = all ((== 1) . length)

    singletonSet :: [String]
    singletonSet = let singletonVals = map head $ filter ((==1) . length) labels
                    in if length singletonVals /= length (nub singletonVals)
                          then error "Repeated singletons"
                          else singletonVals

---- possible labels

possibleLabels :: TicketInfo -> [[String]]
possibleLabels (TicketInfo labels values) = 
  let possibleForValues = (map . map) possible values
   in foldr labelIntersection (head possibleForValues) (tail possibleForValues)
  where 
    possible :: Int -> [String]
    possible value = map ticketLabelLabel $ filter (`validForLabel` value) labels

    labelIntersection :: [[String]] -> [[String]] -> [[String]]
    labelIntersection = zipWith intersect

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

---- ticket parsing

-- data types

data TicketInfo = TicketInfo [TicketLabel] [TicketValues]
data TicketLabel = TicketLabel String Int Int Int Int
type TicketValues = [Int]

myTicketValues :: TicketInfo -> TicketValues
myTicketValues (TicketInfo _ (v:_)) = v

ticketInfoLabels :: TicketInfo -> [TicketLabel]
ticketInfoLabels (TicketInfo ls _) = ls

ticketInfoValues :: TicketInfo -> [TicketValues]
ticketInfoValues (TicketInfo _ vs) = vs

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