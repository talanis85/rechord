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
import Data.ChordPro
import Data.Music.Tonal
import Control.Monad (forM_)
import qualified Data.Text as Text

paperSizeA4 = (595.0, 842.0)

ap3 f (a,b,c) = f a b c

setFont :: LayoutFont -> Render ()
setFont font = do
    selectFontFace (fontFamily font) (fontSlant font) (fontWeight font)
    setFontSize (fontSize $ font)
    setSourceRGB `ap3` (fontColor $ font)

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
      setFontSize (fontSize (chordFont cfg) - 2.0)
      -- relMoveTo 0.0 (-3.0)
      textPath text
      -- relMoveTo 0.0 3.0
      setFontSize (fontSize (chordFont cfg))

    drawSmallSuper text = do
      setFontSize (fontSize (chordFont cfg) - 2.0)
      relMoveTo 0.0 (-3.0)
      textPath text
      relMoveTo 0.0 3.0
      setFontSize (fontSize (chordFont cfg))

renderChord :: LayoutConfig -> TonalChord -> Render (Double)
renderChord cfg chord = do
    setFont (chordFont $ cfg)
    chordPath cfg (show chord)
    fill
    extents <- textExtents (show chord)
    return $ textExtentsXadvance extents

renderMarkup :: LayoutConfig -> Markup -> Render (Double)
renderMarkup cfg markup = do
    setFont (lyricsFont $ cfg)
    extents <- case markup of
        NormalMarkup text -> do
                             textPath text
                             fill
                             textExtents text
        TitleMarkup text  -> do
                             selectFontFace (fontFamily $ lyricsFont cfg) (fontSlant $ lyricsFont cfg) FontWeightBold
                             textPath text
                             fill
                             textExtents (text ++ "   ")
    return $ textExtentsXadvance extents

renderChunk :: LayoutConfig -> Double -> Chunk TonalChord -> Render (Double, Double)
renderChunk cfg lineSize chunk = do
    save
    (w, h) <- case chunk of
        ChunkEmpty -> return (0.0, 0.0)
        ChunkChord chord -> do
            translate 0.0 (fontSize $ chordFont cfg)
            w <- renderChord cfg chord
            return (max (minChunkWidth cfg) w, (fontSize $ chordFont cfg))
        ChunkMarkup markup -> do
            translate 0.0 lineSize
            w <- renderMarkup cfg markup
            return (w, (fontSize $ lyricsFont cfg))
        ChunkBoth chord markup -> do
            translate 0.0 (fontSize $ chordFont cfg)
            w1 <- renderChord cfg chord
            translate 0.0 ((chordSpacing cfg) + (fontSize $ lyricsFont cfg))
            w2 <- renderMarkup cfg markup
            return (max (minChunkWidth cfg) (max w1 w2), (fontSize $ chordFont cfg) + (fontSize $ lyricsFont cfg) + (chordSpacing cfg))
    restore
    return (w, h)

renderLine :: LayoutConfig -> Line TonalChord -> Render ()
renderLine cfg l = do
    save
    renderLine' l 0
    restore
    translate 0.0 (lineSize cfg l + lineSpacing cfg)
  where
        renderLine' [] maxH = return maxH
        renderLine' (c:cs) maxH = do
            (w, h) <- renderChunk cfg (lineSize cfg l) c
            translate (w + (chunkSpacing cfg)) 0.0
            renderLine' cs (max maxH h)

renderParagraph :: LayoutConfig -> Paragraph TonalChord -> Render ()
renderParagraph cfg p = do
    forM_ p $ renderLine cfg
    translate 0.0 (paragraphSpacing cfg)

renderPage :: LayoutConfig -> Layout TonalChord -> Render ()
renderPage cfg paragraphs = do
    save
    forM_ paragraphs $ renderParagraph cfg
    showPage
    restore

renderTitle :: LayoutConfig -> String -> Render ()
renderTitle cfg title = do
    setFont $ titleFont cfg
    textPath title
    fill

renderCairoPDF :: LayoutConfig -> (Double, Double) -> FilePath -> String -> Layout TonalChord -> IO ()
renderCairoPDF cfg (pw, ph) filename title paragraphs = withPDFSurface filename pw ph $ \x -> renderWith x $ do
    let pageSize = (ph - (pageMargin cfg) * 2)
    let pageSize1 = pageSize - (fontSize $ titleFont cfg) - (titleSpacing cfg)
    let (page1:pages) = paginate cfg (pageSize1:(repeat pageSize)) paragraphs
    translate (pageMargin cfg) (pageMargin cfg)
    save
    renderTitle cfg title
    translate 0.0 (pageSize - pageSize1)
    renderPage cfg page1
    restore
    mapM_ (renderPage cfg) pages

data LayoutFont = LayoutFont
    { fontSize :: Double
    , fontFamily :: String
    , fontWeight :: FontWeight
    , fontSlant :: FontSlant
    , fontColor :: (Double, Double, Double)
    }

data LayoutConfig = LayoutConfig
    { chordFont :: LayoutFont
    , lyricsFont :: LayoutFont
    , titleFont :: LayoutFont
    , lineSpacing :: Double
    , chordSpacing :: Double
    , paragraphSpacing :: Double
    , chunkSpacing :: Double
    , minChunkWidth :: Double
    , pageMargin :: Double
    , titleSpacing :: Double
    }

withFont :: String -> LayoutConfig -> LayoutConfig
withFont font cfg = cfg { chordFont = (chordFont cfg) { fontFamily = font }
                        , lyricsFont = (lyricsFont cfg) { fontFamily = font }
                        , titleFont = (titleFont cfg) { fontFamily = font }
                        }

defaultLayoutConfig = LayoutConfig
    { chordFont = LayoutFont
        { fontSize = 13.0
        , fontFamily = "sans-serif"
        , fontWeight = FontWeightBold
        , fontSlant = FontSlantNormal
        , fontColor = (0.5, 0.0, 0.0)
        }
    , lyricsFont = LayoutFont
        { fontSize = 13.0
        , fontFamily = "sans-serif"
        , fontWeight = FontWeightNormal
        , fontSlant = FontSlantNormal
        , fontColor = (0.0, 0.0, 0.0)
        }
    , titleFont = LayoutFont
        { fontSize = 20.0
        , fontFamily = "sans-serif"
        , fontWeight = FontWeightBold
        , fontSlant = FontSlantItalic
        , fontColor = (0.0, 0.0, 0.0)
        }
    , lineSpacing = 6.0
    , chordSpacing = 3.0
    , paragraphSpacing = 20.0
    , chunkSpacing = 3.0
    , minChunkWidth = 40.0
    , pageMargin = 60.0
    , titleSpacing = 15.0
    }

chunkSize :: LayoutConfig -> Chunk TonalChord -> Double
chunkSize cfg c = case c of
    ChunkEmpty             -> 0.0
    ChunkChord chord       -> fontSize $ chordFont cfg
    ChunkMarkup lyrics     -> fontSize $ lyricsFont cfg
    ChunkBoth chord lyrics -> (fontSize $ chordFont cfg) + (fontSize $ lyricsFont cfg) + (chordSpacing cfg)

lineSize :: LayoutConfig -> Line TonalChord -> Double
lineSize cfg l = maximum $ map (chunkSize cfg) l

paragraphSize :: LayoutConfig -> Paragraph TonalChord -> Double
paragraphSize cfg p = (sum $ map (lineSize cfg) p) + (fromIntegral (length p) - 1) * (lineSpacing cfg)

splitParagraph :: LayoutConfig -> Paragraph TonalChord -> Double -> (Paragraph TonalChord, Paragraph TonalChord)
splitParagraph cfg p size = splitParagraph' cfg p size []
    where
        splitParagraph' _ [] _ current = (current, [])
        splitParagraph' cfg (l:ls) size current =
            let linesize = if length ls == 0 then lineSize cfg l else lineSize cfg l + (lineSpacing cfg)
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
            in if paragraphsize + (paragraphSpacing cfg) < size
                  then let newsize = size - paragraphsize - (paragraphSpacing cfg)
                       in paginate' cfg ps (newsize:sizes) (currentPage ++ [p])
                  else if paragraphsize <= size
                          then (currentPage ++ [p]) : paginate' cfg ps sizes []
                          else if length currentPage == 0
                                  then let (a,b) = splitParagraph cfg p size
                                       in paginate' cfg (a:b:ps) (size:sizes) []
                                  else currentPage : paginate' cfg (p:ps) sizes []

