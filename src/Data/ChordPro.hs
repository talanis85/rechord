{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.ChordPro
    ( Chunk (ChunkBoth, ChunkChord, ChunkMarkup, ChunkEmpty)
    , Line, Paragraph, Markup (NormalMarkup, TitleMarkup)
    , Layout
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

data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

type Option = (String, String)

mapLayout :: (a -> b) -> Layout a -> Layout b
mapLayout = map . map . map . fmap

bake :: TonalScale -> Layout DegreeChord -> Layout TonalChord
bake scale = mapLayout $ bakeChord scale
