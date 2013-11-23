{-# LANGUAGE ExistentialQuantification #-}
module Data.ChordPro
    ( Chunk' (ChunkBoth', ChunkChord', ChunkMarkup', ChunkEmpty')
    , Line', Paragraph'
    , Chunk (ChunkBoth, ChunkChord, ChunkMarkup, ChunkEmpty)
    , Line, Paragraph, Markup (NormalMarkup, TitleMarkup)
    -- , transpose
    , bake
    ) where

import Data.Music.Scales
import Data.Music.Chords
import Data.Music.Tonal

-- File layout

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

data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

type Option = (String, String)

bake :: TonalScale -> [Paragraph'] -> [Paragraph]
bake scale = map . map . map $ bakeChunk scale

bakeChunk :: TonalScale -> Chunk' -> Chunk
bakeChunk scale (ChunkBoth' c m) = ChunkBoth (bakeChord scale c) m
bakeChunk scale (ChunkChord' c) = ChunkChord (bakeChord scale c)
bakeChunk scale (ChunkMarkup' m) = ChunkMarkup m
bakeChunk scale ChunkEmpty' = ChunkEmpty

{-
transpose :: Int -> [Paragraph] -> [Paragraph]
transpose amt = map . map . map $ transposeChunk amt

transposeChunk :: Int -> Chunk -> Chunk
transposeChunk amt (ChunkBoth c m) = ChunkBoth (transposeChord amt c) m
transposeChunk amt (ChunkChord c) = ChunkChord (transposeChord amt c)
transposeChunk amt (ChunkMarkup m) = ChunkMarkup m
transposeChunk amt ChunkEmpty = ChunkEmpty
-}
