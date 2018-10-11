import System.Environment
import Rechord.Render.Cairo
import Rechord.Render.HTML
import Text.ChordPro
import Data.ChordPro
import Data.Music.Tonal
import Data.Music.Scales
import qualified Data.Map as M
import Control.Monad (when, forM_)
import System.IO (hGetContents, hSetEncoding, IOMode (..), withFile, utf8)
import System.IO.Error
import System.FilePath
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar

import Text.Printf
import System.Console.GetOpt
import Data.Maybe (isNothing, fromJust, fromMaybe)

data Action = ActionRender | ActionQueryKey

data Options = Options
    { optVersion    :: Bool
    , optHelp       :: Bool
    , optHTML       :: Bool
    , optInput      :: Maybe FilePath
    , optOutput     :: Maybe FilePath
    , optLayout     :: LayoutConfig
    , optTranspose  :: Int
    , optKey        :: Maybe Pitch
    , optAction     :: Action
    }

defaultOptions = Options
    { optVersion = False
    , optHelp = False
    , optHTML = False
    , optInput = Nothing
    , optOutput = Nothing
    , optLayout = defaultLayoutConfig
    , optTranspose = 0
    , optKey = Nothing
    , optAction = ActionRender
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["version"]
        (NoArg (\opts -> opts { optVersion = True }))
        "show version number"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "show this help"
    , Option [] ["html"]
        (NoArg (\opts -> opts { optHTML = True }))
        "html output"
    , Option ['i'] []
        (ReqArg (\f opts -> opts { optInput = Just f }) "FILE")
        "input FILE"
    , Option ['o'] []
        (ReqArg (\f opts -> opts { optOutput = Just f }) "FILE")
        "output FILE"
    , Option ['q'] ["query-key"]
        (NoArg (\opts -> opts { optAction = ActionQueryKey }))
        "Output the sheet's default key"
    , Option ['k'] ["key"]
        (ReqArg (\f opts -> opts { optKey = parsePitch f }) "KEY")
        "key KEY"
    , Option [] ["font"]
        (ReqArg (\f opts -> opts { optLayout = withFont f (optLayout opts) }) "FONT")
        "Font family"
    ]

split c l = split' c [] l
    where
        split' c a [] = [a]
        split' c a (x:xs) = if x == c then a : split' c [] xs else split' c (a ++ [x]) xs

parseOptions argv = do
    case getOpt Permute options argv of
        (o,n,[])    -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: rechord [OPTIONS]"

main = do
    args <- getArgs
    (opts, _) <- parseOptions args

    case optAction opts of
        ActionQueryKey -> do
            when (isNothing $ optInput opts) $ error "No input file specified"
            let infile = fromJust $ optInput opts

            f <- readFile infile
            case parseChordPro f of
                Right (o, k, p) -> putStrLn $ show (tscaleRoot k)
                Left e -> error $ "Error parsing: " ++ (show e)
        ActionRender -> do
            when (isNothing $ optInput opts) $ error "No input file specified"
            let infile = fromJust $ optInput opts
            when (isNothing $ optOutput opts) $ error "No output file specified"
            let outfile = fromJust $ optOutput opts

            f <- withFile infile ReadMode $ \fh -> do
              hSetEncoding fh utf8
              s <- hGetContents fh
              length s `seq` return s
            case parseChordPro f of
                Right (o, k, p) -> let key = TonalScale (fromMaybe (tscaleRoot k) (optKey opts)) (tscaleScale k)
                                   in if optHTML opts
                                         then renderHTML outfile
                                                         (M.findWithDefault "NO TITLE" "t" o)
                                                         (bake key p)
                                         else renderCairoPDF (optLayout opts)
                                                             paperSizeA4
                                                             outfile
                                                             (M.findWithDefault "NO TITLE" "t" o)
                                                             (bake key p)
                Left e' -> error $ "Error parsing: " ++ (show e')
