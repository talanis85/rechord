{-# LANGUAGE GADTs #-}
module Rechord.Render.Disguise
  ( renderCairoPDF
  , defaultLayoutConfig
  , paperSizeA4
  , LayoutConfig (LayoutConfig)
  , FontWeight (FontWeightNormal, FontWeightBold)
  , FontSlant (FontSlantNormal, FontSlantItalic, FontSlantOblique)
  , withFont
  ) where

import Disguise.Cairo hiding (fill)
import qualified Disguise.Cairo
import Disguise.Cairo.PDF
import Graphics.Rendering.Cairo

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Lens
import Data.Char (toLower)
import Data.List (intercalate)
import Control.Monad (forM_)
import qualified Data.Text as Text

import Rechord.Render.Ext
import Rechord.Render.Disguise.Notes

import Data.ChordPro
import Data.Chord
import Data.Pitch

data LayoutFont = LayoutFont
    { _fontSize :: Double
    , _fontFamily :: String
    , _fontWeight :: FontWeight
    , _fontSlant :: FontSlant
    , _fontColor :: (Double, Double, Double)
    }

data LayoutConfig = LayoutConfig
    { _chordFont :: LayoutFont
    , _lyricsFont :: LayoutFont
    , _markFont :: LayoutFont
    , _paragraphFont :: LayoutFont
    , _titleFont :: LayoutFont
    , _headerFont :: LayoutFont
    , _lineSpacing :: Double
    , _chordSpacing :: Double
    , _voiceSpacing :: Double
    , _voiceSize :: Double
    , _barWidth :: Double
    , _paragraphSpacing :: Double
    , _chunkSpacing :: Double
    , _minChordWidth :: Double
    , _minVoiceWidth :: Double
    , _pageMargin :: Double
    , _titleSpacing :: Double
    , _headerSpacing :: Double
    }

makeLenses ''LayoutFont
makeLenses ''LayoutConfig

paperSizeA4 = (595.0, 842.0)

ap3 f (a,b,c) = f a b c

setFont :: LayoutFont -> Render ()
setFont font = do
    selectFontFace (font ^. fontFamily) (font ^. fontSlant) (font ^. fontWeight)
    setFontSize (font ^. fontSize)
    setSourceRGB `ap3` (font ^. fontColor)

chordPath :: LayoutConfig -> String -> Render ()
chordPath cfg = chordPath' cfg
  where
    chordPath' cfg [] = return ()
    chordPath' cfg ('#':xs) = drawSmallSuper "#" >> chordPath' cfg xs
    chordPath' cfg ('b':xs) = drawSmallSuper "b" >> chordPath' cfg xs
    chordPath' cfg ('m':'a':'j':xs) = drawSmallSuper "maj" >> chordPath' cfg xs
    chordPath' cfg ('s':'u':'s':xs) = drawSmallSuper "sus" >> chordPath' cfg xs
    chordPath' cfg ('a':'d':'d':xs) = drawSmallSuper "add" >> chordPath' cfg xs
    chordPath' cfg ('m':xs) = drawSmall "m" >> chordPath' cfg xs
    chordPath' cfg ('+':xs) = drawSmall "+" >> chordPath' cfg xs
    chordPath' cfg ('0':xs) = textPath "\176" >> chordPath' cfg xs
    chordPath' cfg ('d':'i':'m':xs) = textPath "\176" >> chordPath' cfg xs
    chordPath' cfg ('|':xs) = drawSmallSuper "|" >> chordPath' cfg xs
    chordPath' cfg (x:xs)
        | x `elem` ['0'..'9'] = drawSmallSuper [x] >> chordPath' cfg xs
        | otherwise           = textPath [x] >> chordPath' cfg xs

    drawSmall text = do
      setFontSize (cfg ^. chordFont ^. fontSize - 2.0)
      -- relMoveTo 0.0 (-3.0)
      textPath text
      -- relMoveTo 0.0 3.0
      setFontSize (cfg ^. chordFont ^. fontSize)

    drawSmallSuper text = do
      setFontSize (cfg ^. chordFont ^. fontSize - 2.0)
      relMoveTo 0.0 (-3.0)
      textPath text
      relMoveTo 0.0 3.0
      setFontSize (cfg ^. chordFont ^. fontSize)

withFont :: String -> LayoutConfig -> LayoutConfig
withFont font = set (chordFont . fontFamily) font
              . set (lyricsFont . fontFamily) font
              . set (markFont . fontFamily) font
              . set (titleFont . fontFamily) font

defaultLayoutConfig = LayoutConfig
    { _chordFont = LayoutFont
        { _fontSize = 13.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightBold
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.5, 0.0, 0.0)
        }
    , _lyricsFont = LayoutFont
        { _fontSize = 13.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightNormal
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.0, 0.0, 0.0)
        }
    , _markFont = LayoutFont
        { _fontSize = 13.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightBold
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.0, 0.0, 0.0)
        }
    , _paragraphFont = LayoutFont
        { _fontSize = 13.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightBold
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.3, 0.3, 0.3)
        }
    , _titleFont = LayoutFont
        { _fontSize = 20.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightBold
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.0, 0.0, 0.0)
        }
    , _headerFont = LayoutFont
        { _fontSize = 11.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightNormal
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.0, 0.0, 0.0)
        }
    , _lineSpacing = 3.0
    , _chordSpacing = 2.0
    , _voiceSpacing = 3.0
    , _voiceSize = 40.0
    , _barWidth = 5.0
    , _paragraphSpacing = 20.0
    , _chunkSpacing = 3.0
    , _minChordWidth = 40.0
    , _minVoiceWidth = 30.0
    , _pageMargin = 60.0
    , _titleSpacing = 15.0
    , _headerSpacing = 6.0
    }

getTextSize :: LayoutFont -> String -> IO (Double, Double)
getTextSize font str = withImageSurface FormatA1 100 100 $ \s -> renderWith s $ do
  setFont font
  extents <- textExtents str
  return (textExtentsXadvance extents, font ^. fontSize)

getFontSize :: LayoutFont -> IO (Double, Double)
getFontSize font = withImageSurface FormatA1 100 100 $ \s -> renderWith s $ do
  setFont font
  extents <- fontExtents
  return (fontExtentsAscent extents + fontExtentsDescent extents, fontExtentsDescent extents)

-- TODO: Move to disguise-gtk
emptyWidget :: (Applicative f) => CairoWidget (F Dim) (F Dim) f
emptyWidget = FixedWidget $ pure (0, 0, return ())

{-
chunkSize :: LayoutConfig -> Chunk -> IO Double
chunkSize cfg c = case c of
    ChunkEmpty -> return 0.0
    Chunk mus voc lyr ->
      let musSize = maybe (negate (cfg ^. chordSpacing)) (const (cfg ^. chordFont ^. fontSize)) mus
          vocSize = maybe (negate (cfg ^. voiceSpacing)) (const (cfg ^. voiceSize)) voc
          lyrSize = maybe 0.0 (const (cfg ^. lyricsFont ^. fontSize)) lyr
      in return $ musSize + (cfg ^. chordSpacing) + vocSize + (cfg ^. voiceSpacing) + lyrSize
    ChunkExt _ ExtLily src   -> do
      CairoImage img <- generateLily src
      (/ 6) <$> fromIntegral <$> imageSurfaceGetHeight img

lineSize :: LayoutConfig -> Line -> IO Double
lineSize cfg (LineChunks cs) = maximum <$> mapM (chunkSize cfg) cs 
lineSize cfg (LineTitle t) = return $ cfg ^. markFont ^. fontSize
lineSize cfg (LineTitle t) = return $ cfg ^. markFont ^. fontSize
-}

chunk :: (MonadIO f, MonadFix f) => Bool -> Bool -> Bool -> Chunk -> LayoutConfig -> IO (CairoWidget (F Dim) (F Dim) f)
chunk _ _ _ ChunkEmpty cfg = return $ fixh 0 spaceV
chunk hasChords hasVoices hasLyrics (Chunk mus voc lyr) cfg = do
  let vocPart = case (hasVoices, voc) of
        (False, Nothing) -> spaceH
        (False, Just voc') -> error "This should not happen"
        (True, Nothing) -> fixh (cfg ^. voiceSize) emptyNotation
        (True, Just voc') -> fixh (cfg ^. voiceSize) $ voiceNotation voc'
  let musPart = case (hasChords, mus) of
        (False, Nothing) -> emptyWidget
        (False, Just mus') -> error "This should not happen"
        (True, Nothing) -> emptyChord cfg
        (True, Just mus') -> music mus' cfg
  let lyrPart = case (hasLyrics, lyr) of
        (False, Nothing) -> emptyWidget
        (False, Just lyr') -> error "This should not happen"
        (True, Nothing) -> fixh (cfg ^. lyricsFont ^. fontSize) spaceV
        (True, Just lyr') -> markup lyr' cfg
  let spacer = if hasVoices then fixw (cfg ^. minVoiceWidth) spaceH else emptyWidget
  return $ vocPart `topOf` (spacer `topOf` (musPart `topOf` lyrPart))
chunk _ _ _ (ChunkExt _ ExtLily src) cfg = do
  ci@(CairoImage img) <- generateLily src
  h <- imageSurfaceGetHeight img
  return $ fixh (fromIntegral h / 6) (fixedWidthImage ci)

music :: (MonadIO f) => Music -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
music (MusicChord a) = chord a
music MusicBar = bar

chord :: (MonadIO f) => Chord -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
chord a cfg = FixedWidget $ do
  (tw, th) <- liftIO $ getTextSize (cfg ^. chordFont) (show a)
  (fh, fb) <- liftIO $ getFontSize (cfg ^. chordFont)
  return ( max (cfg ^. minChordWidth) tw
         , fh
         , do
           moveTo 0 (fh - fb)
           setFont (cfg ^. chordFont)
           chordPath cfg (show a)
           fill
         )

emptyChord :: (MonadIO f) => LayoutConfig -> CairoWidget (F Dim) (F Dim) f
emptyChord cfg = FixedWidget $ do
  (fh, fb) <- liftIO $ getFontSize (cfg ^. chordFont)
  return (0, fh, return ())

bar :: (MonadIO f) => LayoutConfig -> CairoWidget (F Dim) (F Dim) f
bar cfg = FixedWidget $ do
  (tw, th) <- liftIO $ getTextSize (cfg ^. chordFont) ("|")
  (fh, fb) <- liftIO $ getFontSize (cfg ^. chordFont)
  return ( cfg ^. barWidth
         , fh
         , do
           moveTo 0 (fh - fb)
           setFont (cfg ^. chordFont)
           textPath "|"
           fill
         )

markup' :: (MonadIO f) => String -> LayoutFont -> CairoWidget (F Dim) (F Dim) f
markup' text font = FixedWidget $ do
  (tw, th) <- liftIO $ getTextSize font text
  (fh, fb) <- liftIO $ getFontSize font
  return ( tw
         , fh
         , do
           moveTo 0 (fh - fb)
           setFont font
           textPath text
           fill
         )

markup :: (MonadIO f) => Markup -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
markup (NormalMarkup text) cfg = markup' text (cfg ^. lyricsFont)
markup (TitleMarkup text) cfg = markup' (text ++ "   ") (cfg ^. markFont)

paragraphTitle :: (MonadIO f) => String -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
paragraphTitle text cfg = markup' text (cfg ^. paragraphFont)

title :: (MonadIO f) => String -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
title text cfg = markup' text (cfg ^. titleFont)

subtitle :: (MonadIO f) => String -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
subtitle text cfg = markup' text (cfg ^. headerFont)

lineHasLyrics :: [Chunk] -> Bool
lineHasLyrics = any chunkHasLyrics
  where chunkHasLyrics (Chunk _ _ (Just _)) = True
        chunkHasLyrics _= False

lineHasChords :: [Chunk] -> Bool
lineHasChords = any chunkHasChords
  where chunkHasChords (Chunk (Just _) _ _) = True
        chunkHasChords _= False

lineHasVoices :: [Chunk] -> Bool
lineHasVoices = any chunkHasVoices
  where chunkHasVoices (Chunk _ (Just _) _) = True
        chunkHasVoices _ = False

lineChunks :: (MonadIO f, MonadFix f) => [Chunk] -> LayoutConfig -> IO (CairoWidget (F Dim) (F Dim) f)
lineChunks chunks cfg = do
  lineWidget <- foldl leftOf (fixh 0 spaceV) <$>
    mapM (makeChunk (lineHasChords chunks) (lineHasVoices chunks) (lineHasLyrics chunks)) chunks
  return lineWidget
  where
    makeChunk hasChords hasVoices hasLyrics x = chunk hasChords hasVoices hasLyrics x cfg

paragraph :: (MonadIO f, MonadFix f) => Paragraph -> LayoutConfig -> IO (CairoWidget (V Dim) (F Dim) f)
paragraph lines cfg = foldl topOf spaceH <$> mapM makeLine lines
  where
    makeLine (LineChunks cs) = do
      l <- lineChunks cs cfg
      return $ alignLeft $ l `topOf` fixh (cfg ^. lineSpacing) spaceV
    makeLine (LineTitle t) = do
      return $ Disguise.Cairo.fill (RGB 0.9 0.9 0.9) $ alignLeft $ paragraphTitle t cfg
    makeLine (LineRef t) = do
      return $ Disguise.Cairo.fill (RGB 0.9 0.9 0.9) $ alignLeft $ paragraphTitle ("→ " ++ t) cfg

renderCairoPDF :: LayoutConfig -> (Double, Double) -> FilePath -> String -> String -> [String] -> Layout -> IO ()
renderCairoPDF cfg (pw, ph) filename titleStr subtitleStr headers paragraphs = do
  let pw' = pw - cfg ^. pageMargin * 2
  let ph' = ph - cfg ^. pageMargin * 2

  let header = fixw pw' $
        alignLeft (title titleStr cfg)
        `topOf`
        (space `leftOf` subtitle subtitleStr cfg)
        `topOf`
        fixh (cfg ^. titleSpacing) space

  let makeParagraph x = do
        p <- fixw pw' <$> paragraph x cfg
        return $ p `topOf` fixh (cfg ^. paragraphSpacing) spaceV

  paragraphWidgets <- mapM makeParagraph paragraphs
  pages <- paginate ph' (header : paragraphWidgets)
  withPDFSurface filename pw ph $ \surface -> do
    let renderPage x = do
          case pad (cfg ^. pageMargin) x of
            FixedWidget widget -> do
              renderWith surface $ do
                (_, _, render) <- liftIO widget
                render
                showPage
    mapM_ renderPage pages

paginate :: (MonadIO f, MonadFix f) => Double -> [CairoWidget (F Dim) (F Dim) f] -> f [CairoWidget (F Dim) (F Dim) f]
paginate = paginate' 0 emptyWidget
  where
    paginate' :: (MonadIO f, MonadFix f) => Double -> CairoWidget (F Dim) (F Dim) f -> Double -> [CairoWidget (F Dim) (F Dim) f] -> f [CairoWidget (F Dim) (F Dim) f]
    paginate' curY curWidget ph [] = return [curWidget]
    paginate' curY curWidget ph (x:xs) =
      case x of
        FixedWidget widget -> do
          (w, h, _) <- widget
          if curY + h > ph
             then do
               rest <- paginate ph (x:xs)
               return (curWidget : rest)
             else paginate' (curY + h) (curWidget `topOf` x) ph xs
