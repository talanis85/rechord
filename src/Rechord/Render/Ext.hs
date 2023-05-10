module Rechord.Render.Ext
  ( generateLily
  ) where

import Disguise.Cairo

import System.Directory
import System.Exit
import System.Process
import System.IO

generateLily :: String -> IO CairoImage
generateLily src = do
  tempdir <- getTemporaryDirectory
  (tempf, temph) <- openTempFile tempdir "rechord-chunk"
  hPutStr temph src
  hClose temph
  (code, out, err) <- readCreateProcessWithExitCode (shell $ "lilypond -dresolution=300 -dcrop --png -o " ++ tempf ++ " " ++ tempf) ""
  case code of
    ExitSuccess -> do
      img <- loadImage $ tempf ++ ".cropped.png"
      case img of
        Left err -> error err
        Right img' -> return img'
    ExitFailure n -> do
      hPutStrLn stderr $ "Lilyond failed with code " ++ (show n)
      hPutStrLn stderr "stdout:"
      hPutStrLn stderr out
      hPutStrLn stderr "stderr:"
      hPutStrLn stderr err
      error "unrecoverable"
