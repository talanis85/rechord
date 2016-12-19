module Rechord.Render.HTML
  ( renderHTML
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.ChordPro
import Data.Music.Tonal
import System.IO
import Text.Printf

type Render = ReaderT Handle IO

out :: String -> Render ()
out s = do
  h <- ask
  lift $ hPutStrLn h s

renderTitle :: String -> Render ()
renderTitle t = out $ printf "<h1>%s</h1>" t

renderLayout :: Layout TonalChord -> Render ()
renderLayout = mapM_ renderParagraph

renderParagraph :: Paragraph TonalChord -> Render ()
renderParagraph para = do
  out "<div class=\"paragraph\">"
  mapM_ renderLine para
  out "</div>"

renderLine :: Line TonalChord -> Render ()
renderLine line = do
  out "<div class=\"line\">"
  mapM_ (renderChunk (any isChord line) (any isMarkup line)) line
  out "</div>"

isChord (ChunkChord _) = True
isChord (ChunkBoth _ _) = True
isChord _ = False

isMarkup (ChunkMarkup _) = True
isMarkup (ChunkBoth _ _) = True
isMarkup _ = True

renderChunk :: Bool -> Bool -> Chunk TonalChord -> Render ()
renderChunk padChord padMarkup chunk = do
  out "<div class=\"block\">"
  case chunk of
    ChunkBoth c m -> do
      renderChord c
      renderMarkup m
    ChunkMarkup m -> do
      when padChord $ out "<div class=\"chord\">&nbsp;</div>"
      renderMarkup m
    ChunkChord c -> do
      renderChord c
      when padMarkup $ out "<div class=\"lyrics\">&nbsp;</div>"
    ChunkEmpty -> return ()
  out "</div>"

renderMarkup :: Markup -> Render ()
renderMarkup m = case m of
  NormalMarkup s -> do
    out "<div class=\"lyrics\">"
    out s
    out "</div>"
  TitleMarkup s -> do
    out "<div class=\"text\">"
    out s
    out "</div>"

renderChord :: TonalChord -> Render ()
renderChord c = do
  let c' = show c
  out $ printf "<div class=\"chord\" chord=\"%s\">" c'
  out c'
  out "</div>"

renderPage :: String -> Layout TonalChord -> Render ()
renderPage title layout = do
  out $ printf "<html><head><title>%s</title>" title
  out "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/rechord.css\" />"
  out "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/theme.css\" />"
  out "<script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js\"></script>"
  out "<script type=\"text/javascript\" src=\"js/chordParser.js\"></script>"
  out "<script type=\"text/javascript\" src=\"js/rechord.js\"></script>"
  out "</head>"
  out "<script type=\"text/javascript\">$(document).ready(function () { rechord.renderPage(); });</script>"
  out "<body>"
  renderTitle title
  renderLayout layout
  out "</body></html>"

renderHTML :: FilePath -> String -> Layout TonalChord -> IO ()
renderHTML fp title layout = withFile fp WriteMode $ runReaderT (renderPage title layout)
