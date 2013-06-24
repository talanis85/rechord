module Text.ChordPro
    ( Chord (chordPitch, chordType)
    , Pitch (Pitch)
    , PitchBase (A, B, C, D, E, F, G)
    , PitchMod (Natural, Sharp, Flat)
    , Chunk, Line, Paragraph, Markup (NormalMarkup, TitleMarkup)
    , parseChordPro
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import qualified Data.Map as M

data Chord = Chord
    { chordPitch :: Pitch
    , chordType :: String
    }

instance Show Chord where
    show (Chord p t) = (show p) ++ t

data Pitch = Pitch PitchBase PitchMod

instance Show Pitch where
    show (Pitch b m) = (show b) ++ (show m)

data PitchBase = A | B | C | D | E | F | G
    deriving (Show)
data PitchMod = Natural | Sharp | Flat

instance Show PitchMod where
    show Natural = ""
    show Sharp = "#"
    show Flat = "b"

type Paragraph = [Line]
type Line = [Chunk]
type Chunk = (Maybe Chord, Maybe Markup)
data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

type Option = (String, String)



parseChordPro input = parse cpFile "" input




cpFile = do
    options <- many sheetOption
    many newline
    paras <- paragraph `sepBy` newline
    eof
    return (M.fromList options, paras)

paragraph = many line

line = do
    chunks <- many1 chunk
    newline
    return chunks

chunk = try chunkChordLyric
        <|>
        try chunkChord
        <|>
        try chunkLyric
        <|>
        try chunkTitle

chunkChordLyric = do
    crd <- chord
    lyr <- lyrics
    case dropWhile (`elem` " \t") lyr of
        "" -> return (Just crd, Nothing)
        _ -> return (Just crd, Just (NormalMarkup lyr))

chunkChord = do
    crd <- chord
    return (Just crd, Nothing)

chunkLyric = do
    lyr <- lyrics
    return (Nothing, Just (NormalMarkup lyr))

chunkTitle = do
    char '<'
    tit <- many1 (noneOf ">")
    char '>'
    return (Nothing, Just (TitleMarkup tit))

sheetOption = do
    char '{'
    name <- many (noneOf ":")
    char ':'
    value <- many (noneOf "}")
    char '}'
    newline
    return (name, value)

lyrics = do
    l <- many1 (noneOf "<{[\n")
    return l

chord = do
    char '['
    base <- chordBase
    modifier <- chordMod
    chordType <- many (noneOf "\n]")
    char ']'
    return $ Chord (Pitch base modifier) chordType

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

chordMod = do
    m <- option ' ' (try $ oneOf "b#")
    case m of
        ' ' -> return Natural
        '#' -> return Sharp
        'b' -> return Flat
        _   -> fail $ "Unexpected modifier: " ++ (show m)
