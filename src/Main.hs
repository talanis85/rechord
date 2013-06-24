import System.Environment
import Rechord.Render.Cairo
import Text.ChordPro
import qualified Data.Map as M
import Control.Monad (when)

import System.Console.GetOpt

main = do
    args <- getArgs
    when (length args < 1) $ error "No input file specified"
    when (length args < 2) $ error "No output file specified"
    processFile (args !! 0) (args !! 1)

processFile infile outfile = do
    f <- readFile infile
    case parseChordPro f of
        Right (o, p) -> renderCairoPDF defaultLayoutConfig paperSizeA4 outfile (M.findWithDefault "NO TITLE" "t" o) p
        Left e -> error $ "Error parsing: " ++ (show e)

