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
import Disguise.Cairo.PDF
import Graphics.Rendering.Cairo

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Lens
import Data.ChordPro
import Data.Music.Tonal
import Control.Monad (forM_)
import qualified Data.Text as Text

import Rechord.Render.Ext

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
    , _titleFont :: LayoutFont
    , _headerFont :: LayoutFont
    , _lineSpacing :: Double
    , _chordSpacing :: Double
    , _barWidth :: Double
    , _paragraphSpacing :: Double
    , _chunkSpacing :: Double
    , _minChordWidth :: Double
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
    , _titleFont = LayoutFont
        { _fontSize = 20.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightBold
        , _fontSlant = FontSlantItalic
        , _fontColor = (0.0, 0.0, 0.0)
        }
    , _headerFont = LayoutFont
        { _fontSize = 13.0
        , _fontFamily = "sans-serif"
        , _fontWeight = FontWeightNormal
        , _fontSlant = FontSlantNormal
        , _fontColor = (0.0, 0.0, 0.0)
        }
    , _lineSpacing = 6.0
    , _chordSpacing = 3.0
    , _barWidth = 5.0
    , _paragraphSpacing = 20.0
    , _chunkSpacing = 3.0
    , _minChordWidth = 40.0
    , _pageMargin = 60.0
    , _titleSpacing = 15.0
    , _headerSpacing = 6.0
    }

getTextSize :: LayoutFont -> String -> IO (Double, Double)
getTextSize font str = withImageSurface FormatA1 100 100 $ \s -> renderWith s $ do
  setFont font
  extents <- textExtents str
  return (textExtentsXadvance extents, font ^. fontSize)

-- TODO: Move to disguise-gtk
emptyWidget :: (Applicative f) => CairoWidget (F Dim) (F Dim) f
emptyWidget = FixedWidget $ pure (0, 0, return ())

alignBottom x = space `topOf` x

chunkSize :: LayoutConfig -> Chunk TonalChord -> IO Double
chunkSize cfg c = case c of
    ChunkEmpty             -> return 0.0
    ChunkMusic music       -> return $ cfg ^. chordFont ^. fontSize
    ChunkMarkup lyrics     -> return $ cfg ^. lyricsFont ^. fontSize
    ChunkBoth chord lyrics -> return $ (cfg ^. chordFont ^. fontSize) + (cfg ^. lyricsFont ^. fontSize) + (cfg ^. chordSpacing)
    ChunkExt (ExtLily _) src   -> do
      CairoImage img <- generateLily src
      (/ 6) <$> fromIntegral <$> imageSurfaceGetHeight img

lineSize :: LayoutConfig -> Line TonalChord -> IO Double
lineSize cfg l = maximum <$> mapM (chunkSize cfg) l

chunk :: (MonadIO f, MonadFix f) => Chunk TonalChord -> LayoutConfig -> IO (CairoWidget (F Dim) (V Dim) f)
chunk ChunkEmpty cfg = return spaceV
chunk (ChunkMusic x) cfg = return $ alignTop (music x cfg)
chunk (ChunkMarkup x) cfg = return $ alignBottom (markup x cfg)
chunk (ChunkBoth a b) cfg = return $ alignBottom (music a cfg `topOf` fixh (cfg ^. chordSpacing) spaceV `topOf` markup b cfg)
chunk (ChunkExt (ExtLily _) src) cfg = fixedWidthImage <$> generateLily src

music :: (MonadIO f) => Music TonalChord -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
music (MusicChord a) = chord a
music MusicBar = bar

chord :: (MonadIO f) => TonalChord -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
chord a cfg = FixedWidget $ do
  (tw, th) <- liftIO $ getTextSize (cfg ^. chordFont) (show a)
  return ( max (cfg ^. minChordWidth) tw
         , cfg ^. chordFont ^. fontSize
         , do
           setFont (cfg ^. chordFont)
           chordPath cfg (show a)
           fill
         )

bar :: (Applicative f) => LayoutConfig -> CairoWidget (F Dim) (F Dim) f
bar cfg = FixedWidget $
  pure ( cfg ^. barWidth
       , cfg ^. chordFont ^. fontSize
       , do
         setFont (cfg ^. chordFont)
         textPath "|"
         fill
       )

markup' :: (MonadIO f) => String -> LayoutFont -> CairoWidget (F Dim) (F Dim) f
markup' text font = FixedWidget $ do
  (tw, th) <- liftIO $ getTextSize font text
  return ( tw
         , font ^. fontSize
         , do
           setFont font
           textPath text
           fill
         )

markup :: (MonadIO f) => Markup -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
markup (NormalMarkup text) cfg = markup' text (cfg ^. lyricsFont)
markup (TitleMarkup text) cfg = markup' (text ++ "   ") (cfg ^. markFont)

title :: (MonadIO f) => String -> LayoutConfig -> CairoWidget (F Dim) (F Dim) f
title text cfg = markup' text (cfg ^. titleFont)

line :: (MonadIO f, MonadFix f) => Line TonalChord -> LayoutConfig -> IO (CairoWidget (F Dim) (F Dim) f)
line chunks cfg = do
  ls <- lineSize cfg chunks
  fixh ls <$> foldl leftOf spaceV <$> mapM makeChunk chunks
  where
    makeChunk x = chunk x cfg

paragraph :: (MonadIO f, MonadFix f) => Paragraph TonalChord -> LayoutConfig -> IO (CairoWidget (F Dim) (F Dim) f)
paragraph lines cfg = foldl topOf emptyWidget <$> mapM makeLine lines
  where
    makeLine x = do
      l <- line x cfg
      return $ l `topOf` fixh (cfg ^. lineSpacing) spaceV

renderCairoPDF :: LayoutConfig -> (Double, Double) -> FilePath -> String -> [String] -> Layout TonalChord -> IO ()
renderCairoPDF cfg (pw, ph) filename titleStr headers paragraphs = do
  let pw' = pw - cfg ^. pageMargin * 2
  let ph' = ph - cfg ^. pageMargin * 2

  let header =
        title titleStr cfg
        `topOf`
        fixh (cfg ^. titleSpacing) spaceV

  let makeParagraph x = do
        p <- paragraph x cfg
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
