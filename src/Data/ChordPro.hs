{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.ChordPro
    ( Chunk (..), Music (..)
    , Line, Paragraph, Markup (..)
    , ExtType (..)
    , Layout
    , bake
    , prettyPrintChordPro
    ) where

import Data.Music.Scales
import Data.Music.Chords
import Data.Music.Tonal

import Data.Char (toLower)

-- File layout

type Layout a = [Paragraph a]
type Paragraph a = [Line a]
type Line a = [Chunk a]
data Chunk a = ChunkBoth (Music a) Markup
             | ChunkMusic (Music a)
             | ChunkMarkup Markup
             | ChunkExt ExtType String
             | ChunkEmpty
    deriving (Show, Functor)
data Music a = MusicChord a | MusicBar
    deriving (Show, Functor)

data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

data ExtType = ExtLily TonalScale
    deriving (Show)

type Option = (String, String)

mapLayout :: (a -> b) -> Layout a -> Layout b
mapLayout = map . map . map . fmap

transposeLily :: TonalScale -> Layout a -> Layout a
transposeLily scale x = map (map (map transposeLily')) x
  where
    transposeLily' (ChunkExt (ExtLily scale') src) = ChunkExt (ExtLily scale) $
      "\\transpose " ++ formatLilyPitch (tscaleRoot scale') ++ " " ++ formatLilyPitch (tscaleRoot scale) ++ " { " ++ src ++ " }"
    transposeLily' x = x

formatLilyPitch :: Pitch -> String
formatLilyPitch (Pitch base acc) = formatBase base <> formatAcc acc
  where
    formatBase x = map toLower (show x)
    formatAcc x = mconcat $ map transAcc (show x)
    transAcc '#' = "is"
    transAcc 'b' = "es"
    transAcc _ = ""

bake :: TonalScale -> Layout DegreeChord -> Layout TonalChord
bake scale =
  (mapLayout $ bakeChord scale)
  . transposeLily scale

prettyPrintChordPro :: (Show a) => Layout a -> IO ()
prettyPrintChordPro paras = mapM_ prettyPrintParagraph paras
  where
    prettyPrintParagraph p = do
      putStrLn "PARAGRAPH"
      mapM_ prettyPrintLine p
    prettyPrintLine l = do
      putStrLn "  LINE"
      mapM_ prettyPrintChunk l
    prettyPrintChunk c = do
      putStr "    CHUNK: "
      putStrLn (show c)
