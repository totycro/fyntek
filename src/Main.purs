module Main where

import Prelude (Unit, bind, ($), show, map, (<>), pure)

import Data.String (split, Pattern(..), replace, Replacement(..))
import Data.Decimal (Decimal)
import Data.Decimal (fromString) as Decimal
import Data.Array ((!!))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Console (log)

-- import Effect.Aff (launchAff)
--import Effect.Class (liftEffect)

import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  fileContents <- readInputFile
  log $ show $ parseBankCSVFormat $ parseCSV fileContents


readInputFile :: Effect String
--readInputFile = readTextFile UTF8 "input.csv"
readInputFile = readTextFile UTF8 "/home/totycro/input-split.csv"


-- there are libraries for this, but i'm implementing it as an exercise
parseCSV :: String -> Array (Array String)
parseCSV s =
  let
    lines :: Array String
    lines = split (Pattern "\n") s
    parsedLines :: Array (Array String)
    parsedLines = map (\a -> split (Pattern ";") a) lines
  in
    parsedLines


type Entry = {
  remark :: String,
  date :: String,
  amount :: Decimal
}


parseBankCSVFormat :: Array (Array String) -> Array (Either String Entry)
parseBankCSVFormat lines = map parseLine lines
  where
    parseLine :: Array String -> Either String Entry
    parseLine line = do
       remark <- parseEntry (line !! 1) "remark"
       date <- parseEntry (line !! 2) "date"
       amountStr <- parseEntry (line !! 4) "amount"
       amount <- note ("amount " <> amountStr <> " is not decimal") $ Decimal.fromString $ replace  (Pattern ",") (Replacement ".") amountStr
       pure {
            remark: remark,
            date: date,
            amount: amount
       }

parseEntry :: Maybe String -> String -> Either String String
parseEntry (Just s) _ = Right s
parseEntry (Nothing) what = Left ("Failed to parse " <> what)
