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
import Data.Pitch
import Rechord.Render.Disguise
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
                                          <*> option str (long "font" <> metavar "FONT" <> value "sans serif")
                                          <*> many (option str (short 'h' <> metavar "HEADER"))
                                          <*> option formatP (short 'f' <> metavar "FORMAT" <>  value FormatPdf))
                              (progDesc "Read a message."))
  <> command "parse"    (info (cmdParse <$> argument str (metavar "INPUT"))
                              (progDesc "Only parse (for debugging)"))
  <> command "query"    (info queryP (progDesc "Query information about a sheet"))
  )

maybeOption :: ReadM a -> Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeOption r m = option (Just <$> r) (m <> value Nothing)

queryP :: Parser (IO ())
queryP = hsubparser
  (  command "key"      (info (cmdQueryKey  <$> argument str (metavar "INPUT"))
                              (progDesc "Query the original key"))
  )

pitchP :: ReadM PitchClass
pitchP = maybeReader parsePitch

formatP :: ReadM Format
formatP = maybeReader formatP'
  where
    formatP' "pdf" = Just FormatPdf
    formatP' _ = Nothing

cmdRender :: FilePath -> FilePath -> Maybe PitchClass -> String -> [String] -> Format -> IO ()
cmdRender input output pitch font headers format = do
  withFile input ReadMode $ \fh -> do
    hSetEncoding fh utf8
    f <- hGetContents fh

    case parseChordPro f of
        Right (o, k, p) -> do
          let p' = case pitch of
                     Nothing -> p
                     Just pitch' -> transposeLayout (Pitch k 0 <-> Pitch pitch' 0) p
          case format of
            FormatPdf -> do
              let layout = withFont font defaultLayoutConfig
              renderCairoPDF
                layout
                paperSizeA4
                output
                (Map.findWithDefault "NO TITLE" "t" o)
                headers
                p'
        Left e' -> error $ "Error parsing: " ++ (show e')

cmdParse :: FilePath -> IO ()
cmdParse input = do
  withFile input ReadMode $ \fh -> do
    hSetEncoding fh utf8
    f <- hGetContents fh

    case parseChordPro f of
      Right (o, k, p) -> do
        prettyPrintChordPro p
      Left e' -> error $ "Error parsing: " ++ (show e')

cmdQueryKey :: FilePath -> IO ()
cmdQueryKey fp = do
  f <- readFile fp
  case parseChordPro f of
    Right (o, k, p) -> putStrLn $ show k
    Left e -> error $ "Error parsing: " ++ (show e)

main :: IO ()
main = do
  action <- execParser options
  action
