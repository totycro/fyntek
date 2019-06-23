module Main where

import Prelude (Unit, bind, ($), show, map, (<>), pure, discard)

import Data.String (split, Pattern(..), replace, Replacement(..))
import Data.Decimal (Decimal)
import Data.Eq ((/=))
import Data.Decimal (fromString) as Decimal
import Data.Array ((!!), filter)
-- TODO: nicer import, fromFoldable might be anything
import Data.List (List(..), (:), fromFoldable)
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
  let
    csvData = parseCSV fileContents
    parsedData = parseBankCSVFormat csvData

  log "csv input"
  log $ show csvData
  log "parsed data"
  log $ show parsedData
  log "parse errors"
  log $ show $ errors parsedData


errors :: List (Either String Entry) -> List String
errors (Left x : xs) = x : errors xs
errors (Right _ : xs) = errors xs
errors _ = Nil


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
    parsedLines = map (split (Pattern ";")) lines
  in
    filter (_ /= [""]) parsedLines


type Entry = {
  remark :: String,
  date :: String,
  amount :: Decimal
}


parseBankCSVFormat :: Array (Array String) -> List (Either String Entry)
parseBankCSVFormat lines = toList $ map parseLine lines
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
    toList = fromFoldable


parseEntry :: Maybe String -> String -> Either String String
parseEntry (Just s) _ = Right s
parseEntry (Nothing) what = Left ("Failed to parse " <> what)
