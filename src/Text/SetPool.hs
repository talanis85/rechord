module Text.SetPool
    ( parseSetPool
    ) where

import Text.ParserCombinators.Parsec
import Text.ChordPro
import Data.SetPool
import Data.Music.Tonal
import qualified Data.Map as M

parseSetPool input =
    case parse poolFile "" input of
        Left err -> Left $ show err
        Right v -> Right $ M.fromList v

poolFile = many song

song :: Parser (String, KeyMap)
song = do
    name <- anyChar `manyTill` newline
    entries <- many entry
    return (name, M.fromList entries)

entry :: Parser (String, Pitch)
entry = do
    many1 space
    name <- anyChar `manyTill` (char ':')
    many space
    key <- anyChar `manyTill` newline
    case parsePitch key of
        Nothing -> fail $ "Invalid key for " ++ name
        Just key' -> return (name, key')
