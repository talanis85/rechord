{-# LANGUAGE FlexibleContexts #-}
module Text.ChordPro
    ( parseChordPro
    , parseScale
    , parsePitch
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.Music.Scales
import Data.Music.Chords
import Data.Music.Tonal
import Data.ChordPro
import qualified Data.Map as M


parseChordPro input = runParser cpFile (TonalScale (Pitch C natural) ionianScale) "" input

parseScale input = case parse scaleParser "" input of
                            Left e -> Nothing
                            Right r -> Just r
    where scaleParser = do
            base <- chordBase
            mod <- chordMod
            sc <- majorMinor
            return $ TonalScale (Pitch base mod) sc

parsePitch input = case parse pitchParser "" input of
                            Left e -> Nothing
                            Right r -> Just r
    where pitchParser = do
            base <- chordBase
            mod <- chordMod
            return $ Pitch base mod

cpFile = do
    options <- many sheetOption
    many newline
    paras <- paragraph `sepBy` newline
    eof
    scale <- getState
    return (M.fromList options, scale, paras)

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
        "" -> return $ ChunkChord crd
        _ -> return $ ChunkBoth crd (NormalMarkup lyr)

chunkChord = do
    crd <- chord
    return $ ChunkChord crd

chunkLyric = do
    lyr <- lyrics
    return $ ChunkMarkup (NormalMarkup lyr)

chunkTitle = do
    char '<'
    tit <- many1 (noneOf ">")
    char '>'
    return $ ChunkMarkup (TitleMarkup tit)

sheetOption = do
    char '{'
    name <- many (noneOf ":")
    char ':'
    value <- if name == "key"
                then do
                     keyBase <- chordBase
                     mod <- chordMod
                     scale <- majorMinor
                     let key = TonalScale (Pitch keyBase mod) scale
                     setState key
                     return $ show keyBase ++ show mod -- TODO: geschlecht
                else many (noneOf "}")
    char '}'
    newline
    return (name, value)

lyrics = do
    l <- many1 (noneOf "<{[\n")
    return l

{-
chord = do
    char '['
    base <- chordBase
    modifier <- chordMod
    t <- chordType
    slash <- option (Degree I natural) $ do
                        char '/'
                        slBase <- chordBase
                        slModifier <- chordMod
                        return $ degreeOf (TonalScale (Pitch base modifier) (chordScale t)) (Pitch slBase slModifier)
    char ']'
    return $ TonalChord (Pitch base modifier) (t </> slash)
-}

chord = choice [try absChord, try relChord]

relChord = do
    char '['
    base <- chordDegree
    modifier <- chordMod
    t <- chordType
    slash <- option (Degree I natural) $ do
                        char '/'
                        slBase <- chordDegree
                        slModifier <- chordMod
                        return $ Degree slBase slModifier
    char ']'
    return $ DegreeChord (Degree base modifier) (t </> slash)

absChord = do
    char '['
    base <- chordBase
    modifier <- chordMod
    t <- chordType >>= chordModifiers
    slash <- option (Degree I natural) $ do
                        char '/'
                        slBase <- chordBase
                        slModifier <- chordMod
                        return $ degreeOf (TonalScale (Pitch base modifier) (chordScale t)) (Pitch slBase slModifier)
    char ']'
    scale <- getState
    return $ DegreeChord (degreeOf scale (Pitch base modifier)) (t </> slash)

chordDegree = choice
    [ try (string "III") >> return III
    , try (string "II") >> return II
    , try (string "IV") >> return IV
    , try (string "VII") >> return VII
    , try (string "VI") >> return VI
    , try (string "V") >> return V
    , try (string "I") >> return I
    ]

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
        ' ' -> return natural
        '#' -> return sharp
        'b' -> return flat
        _   -> fail $ "Unexpected modifier: " ++ (show m)

majorMinor = option ionianScale (try $ char 'm' >> return aeolianScale)

{-
chordType = choice
    [ try (string "maj7") >> return major7Chord
    , try (string "maj9") >> return major9Chord
    , try (string "13") >> return (dominant7Chord `addToChord` Degree VI natural)
    , try (string "7b13") >> return (dominant7Chord `addToChord` Degree VI flat)
    , try (string "7b9") >> return (dominant7Chord `addToChord` Degree II flat)
    , try (string "7sus2") >> return (sus2Chord `addToChord` Degree VII flat)
    , try (string "7sus4") >> return (sus4Chord `addToChord` Degree VII flat)
    , try (string "7sus") >> return sus7Chord
    , try (string "7") >> return dominant7Chord
    , try (string "9") >> return dominant9Chord
    , try (string "m7b5") >> return minor7b5Chord
    , try (string "m7") >> return minor7Chord
    , try (string "m9") >> return minor9Chord
    , try (string "m") >> return minorChord
    , try (string "dim") >> return diminishedChord
    , try (string "+") >> return augmentedChord
    , try (string "sus2") >> return sus2Chord
    , try (string "sus4") >> return sus4Chord
    , try (string "sus7") >> return sus7Chord
    , try (string "sus") >> return susChord
    , try (string "6") >> return major6Chord
    , try (string "m6") >> return minor6Chord
    , try (string "add9") >> return (majorChord `addToChord` Degree II natural)
    , try (string "") >> return majorChord
    ]
    -}

chordType = choice
    [ try (string "maj7") >> return major7Chord
    , try (string "mmaj7") >> return minorMajor7Chord
    , try (string "maj9") >> return major9Chord
    , try (string "7") >> return dominant7Chord
    , try (string "9") >> return dominant9Chord
    , try (string "69") >> return major69Chord
    , try (string "6") >> return major6Chord
    , try (string "m69") >> return minor69Chord
    , try (string "m6") >> return minor6Chord
    , try (string "m7") >> return minor7Chord
    , try (string "m9") >> return minor9Chord
    , try (string "m") >> return minorChord
    , try (string "dim") >> return diminishedChord
    , try (string "+") >> return augmentedChord
    , try (string "sus7") >> return sus7Chord
    , return majorChord
    ]

chordModifiers c =
    (choice
        [ try (string "sus2") >> (return $ applySus2 c)
        , try (string "sus4") >> (return $ applySus4 c)
        , try (string "sus") >> (return $ applySus c)
        , try (string "b5") >> (return $ c `without` V `with` Degree V flat)
        , try (string "5") >> (return $ c `without` III)
        , try (string "b9") >> (return $ c `with` Degree II flat)
        , try (string "#9") >> (return $ c `with` Degree II sharp)
        , try (string "add9") >> (return $ c `with` Degree II natural)
        , try (string "9") >> (return $ c `with` Degree VII flat `with` Degree II natural)
        , try (string "7") >> (return $ c `with` Degree VII flat)
        , try (string "maj7") >> (return $ c `with` Degree VII natural)
        , try (string "6") >> (return $ c `with` Degree VI natural)
        , try (string "13") >> (return $ c `with` Degree VII flat `with` Degree VI natural)
        , try (string "b13") >> (return $ c `with` Degree VI flat)
        , try (string "#11") >> (return $ c `with` Degree IV sharp)
        , try (string "11") >> (return $ c `with` Degree IV natural)
        ]
        >>= chordModifiers)
    <|>
    (return c)
