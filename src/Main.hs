import System.Environment
import Rechord.Render.Cairo
import Rechord.Render.HTML
import Text.ChordPro
import Data.ChordPro
import Text.SetPool
import Data.SetPool
import Data.Music.Tonal
import Data.Music.Scales
import qualified Data.Map as M
import Control.Monad (when, forM_)
import System.IO.Error
import System.FilePath
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar

import Text.Printf
import System.Console.GetOpt
import Data.Maybe (isNothing, fromJust, fromMaybe)

data Action = ActionRender | ActionQueryKey | ActionBatch

data Options = Options
    { optVersion    :: Bool
    , optHelp       :: Bool
    , optHTML       :: Bool
    , optInput      :: Maybe FilePath
    , optOutput     :: Maybe FilePath
    , optSingers    :: [String]
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
    , optSingers = []
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
    , Option ['b'] ["batch"]
        (NoArg (\opts -> opts { optAction = ActionBatch }))
        "Batch mode"
    , Option ['k'] ["key"]
        (ReqArg (\f opts -> opts { optKey = parsePitch f }) "KEY")
        "key KEY"
    , Option ['s'] ["singers"]
        (ReqArg (\f opts -> opts { optSingers = split ',' f }) "SINGERS")
        "singer1,singer2,..."
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
{-
                Left e -> case parseEasySheet f of
                    Right (o, k, p) -> let key = TonalScale (fromMaybe (tscaleRoot k) (optKey opts)) (tscaleScale k)
                                       in renderLilyPond (optLayout opts)
                                                         paperSizeA4
                                                         outfile
                                                         (M.findWithDefault "NO TITLE" "t" o)
                                                         (bake key p)
-}
        ActionBatch -> do
            when (isNothing $ optInput opts) $ error "No input directory specified"
            let indir = fromJust $ optInput opts
            when (isNothing $ optOutput opts) $ error "No output directory specified"
            let outdir = fromJust $ optOutput opts
            when (null $ optSingers opts) $ error "No singers specified"
            let singers = optSingers opts

            poolfile <- readFile $ indir </> "pool.txt"
            pooltime <- modTime $ indir </> "pool.txt"

            case parseSetPool poolfile of
                Left err -> error $ "Error parsing poolfile:\n" ++ err
                Right pool -> forM_ (filterSingers singers pool) $ \(song, singer, pitch) -> do
                    let infile = indir </> (song ++ ".crd")
                    f <- tryIOError $ readFile infile
                    case f of
                        Left _ -> putStrLn $ "MISS " ++ song
                        Right f' -> do
                            let outfile = outdir </> (song ++ ".pdf")

                            intime <- modTime infile
                            outtime <- modTime outfile

                            if intime < outtime && pooltime < outtime
                                then putStrLn $ "UNCH " ++ song
                                else case parseChordPro f' of
                                    Left err -> do
                                              putStrLn $ "PARS " ++ song
                                              putStr $ unlines $ map ("> " ++) $ lines $ show err
                                    Right (o, k, p) -> do
                                        let key = TonalScale pitch (tscaleScale k)
                                        let minmaj = if tscaleScale k `scaleHas` Degree III flat then "m" else ""
                                        let title = printf "%s (%s, %s%s)" (M.findWithDefault "NO TITLE" "t" o) singer (show pitch) minmaj
                                        if optHTML opts
                                           then renderHTML outfile
                                                           title
                                                           (bake key p)
                                           else renderCairoPDF (optLayout opts)
                                                               paperSizeA4
                                                               outfile
                                                               title
                                                               (bake key p)
                                        putStrLn $ "OK   " ++ song

modTime :: FilePath -> IO UTCTime
modTime p = do
    r <- tryIOError (getModificationTime p)
    case r of
        Left _ -> return $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        Right t -> return t
