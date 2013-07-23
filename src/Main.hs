import System.Environment
import Rechord.Render.Cairo
import Text.ChordPro
import Data.ChordPro
import qualified Data.Map as M
import Control.Monad (when)

import System.Console.GetOpt
import Data.Maybe (isNothing, fromJust)

data Options = Options
    { optVersion    :: Bool
    , optHelp       :: Bool
    , optInput      :: Maybe FilePath
    , optOutput     :: Maybe FilePath
    , optLayout     :: LayoutConfig
    , optTranspose  :: Int
    }

defaultOptions = Options
    { optVersion = False
    , optHelp = False
    , optInput = Nothing
    , optOutput = Nothing
    , optLayout = defaultLayoutConfig
    , optTranspose = 0
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["version"]
        (NoArg (\opts -> opts { optVersion = True }))
        "show version number"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "show this help"
    , Option ['i'] []
        (ReqArg (\f opts -> opts { optInput = Just f }) "FILE")
        "input FILE"
    , Option ['o'] []
        (ReqArg (\f opts -> opts { optOutput = Just f }) "FILE")
        "output FILE"
    , Option ['t'] ["transpose"]
        (ReqArg (\f opts -> opts { optTranspose = (read f) }) "SEMITONES")
        "transpose [+-]SEMITONES"
    ]

parseOptions argv = do
    case getOpt Permute options argv of
        (o,n,[])    -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: rechord [OPTIONS]"

main = do
    args <- getArgs
    (opts, _) <- parseOptions args

    when (isNothing $ optInput opts) $ error "No input file specified"
    let infile = fromJust $ optInput opts
    when (isNothing $ optOutput opts) $ error "No output file specified"
    let outfile = fromJust $ optOutput opts

    f <- readFile infile
    case parseChordPro f of
        Right (o, p) -> renderCairoPDF defaultLayoutConfig paperSizeA4 outfile (M.findWithDefault "NO TITLE" "t" o) $ 
                            transpose (optTranspose opts) p
        Left e -> error $ "Error parsing: " ++ (show e)

