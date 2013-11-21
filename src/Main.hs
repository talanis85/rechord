import System.Environment
import Rechord.Render.Cairo
import Text.ChordPro
import Data.ChordPro
import Data.Music.Tonal
import qualified Data.Map as M
import Control.Monad (when)

import System.Console.GetOpt
import Data.Maybe (isNothing, fromJust, fromMaybe)

data Action = ActionRender | ActionQueryKey

data Options = Options
    { optVersion    :: Bool
    , optHelp       :: Bool
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

            f <- readFile infile
            case parseChordPro f of
                Right (o, k, p) -> {- 
                                   let transposition = if optTranspose opts == 0
                                                       then (read $ M.findWithDefault "0" "tp" o)
                                                       else optTranspose opts
                                       in renderCairoPDF defaultLayoutConfig paperSizeA4 outfile (M.findWithDefault "NO TITLE" "t" o) $ 
                                           transpose transposition (bake key p)
                                   -}
                                   let key = TonalScale (fromMaybe (tscaleRoot k) (optKey opts)) (tscaleScale k)
                                   in renderCairoPDF defaultLayoutConfig
                                                     paperSizeA4
                                                     outfile
                                                     (M.findWithDefault "NO TITLE" "t" o)
                                                     (bake key p)
                Left e -> error $ "Error parsing: " ++ (show e)
