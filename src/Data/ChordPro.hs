{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.ChordPro
    ( Chunk (..), Music (..)
    , Line, Paragraph, Markup (..)
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
data Chunk a = ChunkBoth (Music a) Markup
             | ChunkMusic (Music a)
             | ChunkMarkup Markup
             | ChunkEmpty
    deriving (Show, Functor)
data Music a = MusicChord a | MusicBar
    deriving (Show, Functor)

data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

type Option = (String, String)

mapLayout :: (a -> b) -> Layout a -> Layout b
mapLayout = map . map . map . fmap

bake :: TonalScale -> Layout DegreeChord -> Layout TonalChord
bake scale = mapLayout $ bakeChord scale
