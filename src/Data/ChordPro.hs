{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.ChordPro
    ( Chunk (ChunkBoth, ChunkChord, ChunkMarkup, ChunkEmpty)
    , Line, Paragraph, Markup (NormalMarkup, TitleMarkup)
    , Layout
    -- , transpose
    , bake
    ) where

import Data.Music.Scales
import Data.Music.Chords
import Data.Music.Tonal

-- File layout

type Layout a = [Paragraph a]
type Paragraph a = [Line a]
type Line a = [Chunk a]
data Chunk a = ChunkBoth a Markup
             | ChunkChord a
             | ChunkMarkup Markup
             | ChunkEmpty
    deriving (Show, Functor)

{-
type Paragraph' = [Line']
type Line' = [Chunk']
data Chunk' = ChunkBoth' DegreeChord Markup
           | ChunkChord' DegreeChord
           | ChunkMarkup' Markup
           | ChunkEmpty'
    deriving (Show)

type Paragraph = [Line]
type Line = [Chunk]
data Chunk = ChunkBoth TonalChord Markup
           | ChunkChord TonalChord
           | ChunkMarkup Markup
           | ChunkEmpty
-}

data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

type Option = (String, String)

mapLayout :: (a -> b) -> Layout a -> Layout b
mapLayout = map . map . map . fmap

bake :: TonalScale -> Layout DegreeChord -> Layout TonalChord
bake scale = mapLayout $ bakeChord scale

{-
bakeChunk :: TonalScale -> Chunk DegreeChord -> Chunk TonalChord
bakeChunk scale = fmap (bakeChord scale)
-}

{-
transpose :: Int -> [Paragraph] -> [Paragraph]
transpose amt = map . map . map $ transposeChunk amt

transposeChunk :: Int -> Chunk -> Chunk
transposeChunk amt (ChunkBoth c m) = ChunkBoth (transposeChord amt c) m
transposeChunk amt (ChunkChord c) = ChunkChord (transposeChord amt c)
transposeChunk amt (ChunkMarkup m) = ChunkMarkup m
transposeChunk amt ChunkEmpty = ChunkEmpty
-}
