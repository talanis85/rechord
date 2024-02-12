{-# LANGUAGE FlexibleContexts #-}
module Text.ChordPro
    ( parseChordPro
    , parsePitch
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.ChordPro
import Data.Chord
import Data.Pitch
import qualified Data.Map as M

parseChordPro input = runParser cpFile (PitchClass C 0) "" input

parsePitch input = case parse pitchParser "" input of
                            Left e -> Nothing
                            Right r -> Just r
    where pitchParser = do
            base <- chordBase
            mod <- chordMod
            return $ PitchClass base mod

cpFile = do
    options <- many sheetOption
    many newline
    paras <- paragraph `sepBy` newline
    eof
    basenote <- getState
    return (M.fromList options, basenote, paras)

paragraph = many line

line = try lineTitle <|> lineChunks

lineChunks = do
    chunks <- many1 chunk
    newline
    return $ LineChunks chunks

lineTitle = do
    char '<'
    tit <- many1 (noneOf ">")
    char '>'
    newline
    return $ LineTitle tit

chunk = try chunkExtLily
        <|>
        try chunkTitle
        <|>
        try chunk'

chunk' = do
    mus <- optionMaybe (try music)
    voc <- optionMaybe (try voices)
    lyr <- optionMaybe (try lyrics)
    case (mus, voc, lyr) of
      (Nothing, Nothing, Nothing) -> fail "Empty chunk"
      _ -> return $ Chunk mus voc (NormalMarkup <$> lyr)

voices = do
  char '{'
  ret <- voice `sepBy1` space
  char '}'
  return $ zip [0..] ret

voice = do
  base <- choice
    [ char 'A' >> return A
    , char 'B' >> return B
    , char 'C' >> return C
    , char 'D' >> return D
    , char 'E' >> return E
    , char 'F' >> return F
    , char 'G' >> return G
    ]
  acc <- option 0 ((char '#' >> return 1) <|> (char 'b' >> return (-1)))
  oct <- option 0 ((char ',' >> return (-1)) <|> (char '\'' >> return 1))
  return (Pitch (PitchClass base acc) oct)

chunkExtLily = do
    string "<lily>"
    source <- manyTill anyChar (try (string "</lily>"))
    basenote <- getState
    return $ ChunkExt basenote ExtLily source

chunkTitle = do
    char '<'
    tit <- many1 (noneOf ">")
    char '>'
    return $ Chunk Nothing Nothing (Just (TitleMarkup tit))

sheetOption = do
    char '{'
    name <- many (noneOf ":")
    char ':'
    value <- if name == "key"
                then do
                     keyBase <- chordBase
                     mod <- chordMod
                     _ <- option 'M' (char 'm')
                     setState (PitchClass keyBase mod)
                     return $ show keyBase ++ show mod
                else many (noneOf "}")
    char '}'
    newline
    return (name, value)

lyrics = do
    l <- many1 (noneOf "|<{[\n")
    return l

music = choice [try musicBar, try musicChord]

musicBar = do
    skipMany (oneOf " \t")
    char '|'
    skipMany (oneOf " \t")
    return MusicBar

musicChord = do
    crd <- chord
    return $ MusicChord crd

chord = absChord <* many (oneOf " \t")

absChord = do
    char '['
    base <- chordBase
    modifier <- chordMod
    t1 <- many (noneOf "1234567/]")
    t2 <- many (noneOf "/]")
    slash <- optionMaybe $ do
                        char '/'
                        slBase <- chordBase
                        slModifier <- chordMod
                        return $ PitchClass slBase slModifier
    char ']'
    return (Chord (PitchClass base modifier) slash (ChordType t1 t2))

{-
chordDegree = choice
    [ try (string "III") >> return III
    , try (string "II") >> return II
    , try (string "IV") >> return IV
    , try (string "VII") >> return VII
    , try (string "VI") >> return VI
    , try (string "V") >> return V
    , try (string "I") >> return I
    ]
-}

chordBase = do
    n <- oneOf "ABCDEFG"
    case n of
        'A' -> return A
        'B' -> return B
        'C' -> return C
        'D' -> return D
        'E' -> return E
        'F' -> return F
        'G' -> return G
        _   -> fail $ "Unexpected chord name: " ++ (show n)

chordMod = foldr (+) 0 <$> many (try ((char 'b' >> return (-1)) <|> (char '#' >> return 1)))
