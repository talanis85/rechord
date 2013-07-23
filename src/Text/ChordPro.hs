module Text.ChordPro
    ( parseChordPro
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.ChordPro
import qualified Data.Map as M


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
    chordType <- many (noneOf "/\n]")
    slash <- option Nothing $ do
                              char '/'
                              base <- chordBase
                              modifier <- chordMod
                              return $ Just $ Pitch base modifier
    char ']'
    return $ Chord (Pitch base modifier) chordType slash

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
