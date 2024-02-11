{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.ChordPro
    ( Chunk (..), Music (..)
    , Line, Paragraph, Markup (..)
    , ExtType (..)
    , Layout
    , transposeLayout
    , prettyPrintChordPro
    ) where

import Data.Bifunctor
import Data.Char (toLower)

import Data.Chord
import Data.Pitch

-- File layout

type Layout = [Paragraph]
type Paragraph = [Line]
type Line = [Chunk]
data Chunk = Chunk (Maybe Music) (Maybe [(Int, Pitch)]) (Maybe Markup)
             | ChunkExt PitchClass ExtType String
             | ChunkEmpty
    deriving (Show)
data Music = MusicChord Chord | MusicBar
    deriving (Show)

data Markup = NormalMarkup String | TitleMarkup String
    deriving (Show)

data ExtType = ExtLily
    deriving (Show)

type Option = (String, String)

transposeLayout :: Directional Interval -> Layout -> Layout
transposeLayout i x = map (map (map (transposeChunk i))) x

transposeChunk :: Directional Interval -> Chunk -> Chunk
transposeChunk i x = case x of
  Chunk mus voc lyr ->
    let mus' = fmap (transposeMusic i) mus
        voc' = fmap (map (second (<+> i))) voc
        lyr' = lyr
    in Chunk mus' voc' lyr'
  ChunkExt pc ExtLily s ->
    let pc' = pitchClass (Pitch pc 0 <+> i)
        s' = "\\transpose " ++ formatLilyPitch pc ++ " " ++ formatLilyPitch pc' ++ " { " ++ s ++ " }"
    in ChunkExt pc' ExtLily s'
  ChunkEmpty -> ChunkEmpty

transposeMusic :: Directional Interval -> Music -> Music
transposeMusic i (MusicChord c) = MusicChord (transposeChord i c)
transposeMusic _ x = x

transposeChord :: Directional Interval -> Chord -> Chord
transposeChord i = mapChordPitch (<+> i)

formatLilyPitch :: PitchClass -> String
formatLilyPitch (PitchClass base acc) = formatBase base <> formatAcc acc
  where
    formatBase x = map toLower (show x)
    formatAcc x = case signum x of
                    0 -> ""
                    1 -> concat $ take x (repeat "is")
                    (-1) -> concat $ take (abs x) (repeat "es")

prettyPrintChordPro :: Layout -> IO ()
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
