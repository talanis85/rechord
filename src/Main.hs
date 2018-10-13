module Main where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Development.GitRev
import Options.Applicative
import System.FilePath
import System.IO (hGetContents, hSetEncoding, IOMode (..), withFile, utf8)
import System.IO.Error

import Data.ChordPro
import Data.Music.Scales
import Data.Music.Tonal
import Rechord.Render.Cairo
import Rechord.Render.HTML
import Text.ChordPro

options :: ParserInfo (IO ())
options = info (helper <*> commandP)
  (  fullDesc
  <> progDesc "Render ChordPro sheets"
  <> header "rechord - A ChordPro renderer"
  <> footer ("Version: " ++ version)
  )

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

data Format = FormatPdf | FormatHtml

commandP :: Parser (IO ())
commandP = hsubparser
  (  command "render"   (info (cmdRender  <$> argument str (metavar "INPUT")
                                          <*> argument str (metavar "OUTPUT")
                                          <*> maybeOption pitchP (short 'k' <> metavar "KEY")
                                          <*> option auto (short 't' <> metavar "TRANSPOSITION" <> value 0)
                                          <*> option str (long "font" <> metavar "FONT" <> value "sans serif")
                                          <*> option formatP (short 'f' <> metavar "FORMAT" <>  value FormatPdf))
                              (progDesc "Read a message."))
  <> command "query"    (info queryP (progDesc "Query information about a sheet"))
  )

maybeOption :: ReadM a -> Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOption r m = option (Just <$> r) (m <> value Nothing)

queryP :: Parser (IO ())
queryP = hsubparser
  (  command "key"      (info (cmdQueryKey  <$> argument str (metavar "INPUT"))
                              (progDesc "Query the original key"))
  )

pitchP :: ReadM Pitch
pitchP = maybeReader parsePitch

formatP :: ReadM Format
formatP = maybeReader formatP'
  where
    formatP' "html" = Just FormatHtml
    formatP' "pdf" = Just FormatPdf
    formatP' _ = Nothing

cmdRender :: FilePath -> FilePath -> Maybe Pitch -> Int -> String -> Format -> IO ()
cmdRender input output pitch transposition font format = do
  withFile input ReadMode $ \fh -> do
    hSetEncoding fh utf8
    f <- hGetContents fh

    case parseChordPro f of
        Right (o, k, p) -> do
          let key = TonalScale (fromMaybe (tscaleRoot k) pitch) (tscaleScale k)
          case format of
            FormatHtml ->
              renderHTML
                output
                (Map.findWithDefault "NO TITLE" "t" o)
                (bake key p)
            FormatPdf -> do
              let layout = withFont font defaultLayoutConfig
              renderCairoPDF
                layout
                paperSizeA4
                output
                (Map.findWithDefault "NO TITLE" "t" o)
                (bake key p)
        Left e' -> error $ "Error parsing: " ++ (show e')

cmdQueryKey :: FilePath -> IO ()
cmdQueryKey fp = do
  f <- readFile fp
  case parseChordPro f of
    Right (o, k, p) -> putStrLn $ show (tscaleRoot k)
    Left e -> error $ "Error parsing: " ++ (show e)

main :: IO ()
main = do
  action <- execParser options
  action
