module Rechord.Render.Cairo
    ( renderCairoPDF
    , defaultLayoutConfig
    , paperSizeA4
    , LayoutConfig (LayoutConfig)
    , FontWeight (FontWeightNormal, FontWeightBold)
    , FontSlant (FontSlantNormal, FontSlantItalic, FontSlantOblique)
    , withFont
    ) where

import Graphics.Rendering.Cairo
import Control.Lens
import Data.ChordPro
import Data.Music.Tonal
import Control.Monad (forM_)
import qualified Data.Text as Text

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

renderMusic :: LayoutConfig -> Music TonalChord -> Render (Double)
renderMusic cfg (MusicChord chord) = renderChord cfg chord
renderMusic cfg MusicBar = renderBar cfg

renderBar :: LayoutConfig -> Render (Double)
renderBar cfg = do
    setFont (cfg ^. chordFont)
    setFontSize (cfg ^. chordFont ^. fontSize)
    textPath "|"
    fill
    extents <- textExtents "|"
    return (cfg ^. barWidth)

renderChord :: LayoutConfig -> TonalChord -> Render (Double)
renderChord cfg chord = do
    setFont (cfg ^. chordFont)
    chordPath cfg (show chord)
    fill
    extents <- textExtents (show chord)
    return (max (cfg ^. minChordWidth) (textExtentsXadvance extents))

renderMarkup :: LayoutConfig -> Markup -> Render (Double)
renderMarkup cfg markup = do
    setFont (cfg ^. lyricsFont)
    extents <- case markup of
        NormalMarkup text -> do
                             textPath text
                             fill
                             textExtents text
        TitleMarkup text  -> do
                             selectFontFace (cfg ^. lyricsFont ^. fontFamily)
                                            (cfg ^. lyricsFont ^. fontSlant)
                                            FontWeightBold
                             textPath text
                             fill
                             textExtents (text ++ "   ")
    return $ textExtentsXadvance extents

renderChunk :: LayoutConfig -> Double -> Chunk TonalChord -> Render (Double, Double)
renderChunk cfg lineSize chunk = do
    save
    (w, h) <- case chunk of
        ChunkEmpty -> return (0.0, 0.0)
        ChunkMusic music -> do
            translate 0.0 (cfg ^. chordFont ^. fontSize)
            w <- renderMusic cfg music
            return (w, (cfg ^. chordFont ^. fontSize))
        ChunkMarkup markup -> do
            translate 0.0 lineSize
            w <- renderMarkup cfg markup
            return (w, (cfg ^. lyricsFont ^. fontSize))
        ChunkBoth music markup -> do
            translate 0.0 (cfg ^. chordFont ^. fontSize)
            w1 <- renderMusic cfg music
            translate 0.0 ((cfg ^. chordSpacing) + (cfg ^. lyricsFont ^. fontSize))
            w2 <- renderMarkup cfg markup
            return (   (max w1 w2)
                   ,   (cfg ^. chordFont ^. fontSize)
                     + (cfg ^. lyricsFont ^. fontSize)
                     + (cfg ^. chordSpacing)
                   )
    restore
    return (w, h)

renderLine :: LayoutConfig -> Line TonalChord -> Render ()
renderLine cfg l = do
    save
    renderLine' l 0
    restore
    translate 0.0 (lineSize cfg l + cfg ^. lineSpacing)
  where
        renderLine' [] maxH = return maxH
        renderLine' (c:cs) maxH = do
            (w, h) <- renderChunk cfg (lineSize cfg l) c
            translate (w + (cfg ^. chunkSpacing)) 0.0
            renderLine' cs (max maxH h)

renderParagraph :: LayoutConfig -> Paragraph TonalChord -> Render ()
renderParagraph cfg p = do
    forM_ p $ renderLine cfg
    translate 0.0 (cfg ^. paragraphSpacing)

renderPage :: LayoutConfig -> Layout TonalChord -> Render ()
renderPage cfg paragraphs = do
    save
    forM_ paragraphs $ renderParagraph cfg
    showPage
    restore

renderTitle :: LayoutConfig -> String -> Render ()
renderTitle cfg title = do
    setFont $ cfg ^. titleFont
    textPath title
    fill

renderHeaders :: LayoutConfig -> Double -> [String] -> Render ()
renderHeaders cfg pw headers = mapM_ renderHeader headers
  where
    renderHeader header = do
      save
      setFont $ cfg ^. headerFont
      extents <- textExtents header
      translate (pw - textExtentsXadvance extents - (cfg ^. pageMargin) * 2) 0.0
      textPath header
      fill
      restore
      translate 0.0 (cfg ^. headerFont ^. fontSize + cfg ^. headerSpacing)

renderCairoPDF :: LayoutConfig -> (Double, Double) -> FilePath -> String -> [String] -> Layout TonalChord -> IO ()
renderCairoPDF cfg (pw, ph) filename title headers paragraphs = withPDFSurface filename pw ph $ \x -> renderWith x $ do
    let pageSize = (ph - (cfg ^. pageMargin) * 2)
    let headersSize = (cfg ^. headerFont ^. fontSize) * fromIntegral (length headers)
    let pageSize1 = pageSize - (cfg ^. titleFont ^. fontSize) - (cfg ^. titleSpacing)
    let (page1:pages) = paginate cfg (pageSize1:(repeat pageSize)) paragraphs
    translate (cfg ^. pageMargin) (cfg ^. pageMargin)
    save
    save
    renderTitle cfg title
    translate 0.0 (cfg ^. titleFont ^. fontSize)
    renderHeaders cfg pw headers
    restore
    translate 0.0 (pageSize - pageSize1)
    renderPage cfg page1
    restore
    mapM_ (renderPage cfg) pages

withFont :: String -> LayoutConfig -> LayoutConfig
withFont font = set (chordFont . fontFamily) font
              . set (lyricsFont . fontFamily) font
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

chunkSize :: LayoutConfig -> Chunk TonalChord -> Double
chunkSize cfg c = case c of
    ChunkEmpty             -> 0.0
    ChunkMusic music       -> cfg ^. chordFont ^. fontSize
    ChunkMarkup lyrics     -> cfg ^. lyricsFont ^. fontSize
    ChunkBoth chord lyrics -> (cfg ^. chordFont ^. fontSize) + (cfg ^. lyricsFont ^. fontSize) + (cfg ^. chordSpacing)

lineSize :: LayoutConfig -> Line TonalChord -> Double
lineSize cfg l = maximum $ map (chunkSize cfg) l

paragraphSize :: LayoutConfig -> Paragraph TonalChord -> Double
paragraphSize cfg p = (sum $ map (lineSize cfg) p) + (fromIntegral (length p) - 1) * (cfg ^. lineSpacing)

splitParagraph :: LayoutConfig -> Paragraph TonalChord -> Double -> (Paragraph TonalChord, Paragraph TonalChord)
splitParagraph cfg p size = splitParagraph' cfg p size []
    where
        splitParagraph' _ [] _ current = (current, [])
        splitParagraph' cfg (l:ls) size current =
            let linesize = if length ls == 0 then lineSize cfg l else lineSize cfg l + (cfg ^. lineSpacing)
            in if linesize < size
                  then splitParagraph' cfg ls (size - linesize) (current ++ [l])
                  else (current, (l:ls))

paginate :: LayoutConfig -> [Double] -> Layout TonalChord -> [Layout TonalChord]
paginate cfg sizes paragraphs = paginate' cfg paragraphs sizes []
    where
        paginate' _ [] _ cur = case cur of
                                [] -> []
                                x  -> [x]
        paginate' _ _ [] cur = [cur]
        paginate' cfg (p:ps) (size:sizes) currentPage =
            let paragraphsize = paragraphSize cfg p
            in if paragraphsize + (cfg ^. paragraphSpacing) < size
                  then let newsize = size - paragraphsize - (cfg ^. paragraphSpacing)
                       in paginate' cfg ps (newsize:sizes) (currentPage ++ [p])
                  else if paragraphsize <= size
                          then (currentPage ++ [p]) : paginate' cfg ps sizes []
                          else if length currentPage == 0
                                  then let (a,b) = splitParagraph cfg p size
                                       in paginate' cfg (a:b:ps) (size:sizes) []
                                  else currentPage : paginate' cfg (p:ps) sizes []

