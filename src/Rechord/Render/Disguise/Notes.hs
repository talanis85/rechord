module Rechord.Render.Disguise.Notes
  ( emptyNotation
  , voiceNotation
  ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

import Disguise.Cairo hiding (fill, scale)
import Graphics.Rendering.Cairo

import Data.Pitch

emptyNotation :: (MonadIO f) => CairoWidget (V Dim) (V Dim) f
emptyNotation = FlowWidget $ \w h -> do
  let sysh = 0.5 * h
      padtop = 0.25 * h
      padbot = 0.25 * h

  return $ do
    setLineWidth (sysh / 60.0)
    setSourceRGB 0.4 0.4 0.4 
    forM_ [0, sysh / 4.0, 2 * sysh / 4.0, 3 * sysh / 4.0, sysh] $ \x -> do
      moveTo 0 (padtop + x)
      lineTo w (padtop + x)
      stroke

voiceNotation :: (MonadIO f) => [(Int, Pitch)] -> CairoWidget (V Dim) (V Dim) f
voiceNotation notes = FlowWidget $ \w h -> do
  let sysh = 0.5 * h
      padtop = 0.25 * h
      padbot = 0.25 * h
      noteEllipsisFactor = 1.2
      noteWidth = (sysh / 4.0) * noteEllipsisFactor

  return $ do
    setLineWidth (sysh / 60.0)
    setSourceRGB 0.4 0.4 0.4 
    forM_ [0, sysh / 4.0, 2 * sysh / 4.0, 3 * sysh / 4.0, sysh] $ \x -> do
      moveTo 0 (padtop + x)
      lineTo w (padtop + x)
      stroke

    let baseline = padtop + sysh + (sysh / 4.0)
    let topAux = pitchAuxLinesTreble (maximum (map snd notes))
    let botAux = abs (pitchAuxLinesTreble (minimum (map snd notes)))

    setSourceRGB 0 0 0 

    when (topAux > 0) $ do
      setLineWidth (sysh / 40.0)
      setSourceRGB 0.4 0.4 0.4 
      forM_ [1..topAux] $ \x -> do
        moveTo 0 (padtop - fromIntegral x * (sysh / 4.0))
        lineTo (noteWidth * 2) (padtop - fromIntegral x * (sysh / 4.0))
        stroke

    when (botAux > 0) $ do
      setLineWidth (sysh / 40.0)
      setSourceRGB 0.4 0.4 0.4 
      forM_ [1..botAux] $ \x -> do
        moveTo 0 (baseline + fromIntegral (x-1) * (sysh / 4.0))
        lineTo (noteWidth * 2) (baseline + fromIntegral (x-1) * (sysh / 4.0))
        stroke

    forM_ notes $ \(i, pitch) -> do
      case i of
        0 -> setSourceRGB 0 0 0 
        1 -> setSourceRGB 0.7 0 0 
        2 -> setSourceRGB 0 0.5 0 
        _ -> setSourceRGB 0 0 0.5

      save
      let y = fromIntegral (pitchPosition pitch) * (sysh / 8.0)
      scale noteEllipsisFactor 1
      accPos <- if any (\(_, pitch') -> intervalBase (eitherDirection (pitch' <-> pitch)) == 1) notes
         && even (pitchPosition pitch)
         then do
              translate (sysh / 4.0 + sysh / 8.0) (baseline - y)
              return (sysh / 4.0)
         else do
              translate (sysh / 4.0) (baseline - y)
              return 0
      rotate (pi / 4)
      scale 0.7 1.1
      arc 0 0 (sysh / 8.0) 0 (2 * pi)
      fill
      restore

      let acc = pitchAccidental pitch
      when (acc > 0) $ do
        moveTo (negate (fromIntegral acc * sysh / 4.0) - accPos) (baseline - y + sysh / 8.0)
        setFontSize (noteWidth * 1.5)
        textPath (take acc (repeat '#'))
        fill

      when (acc < 0) $ do
        moveTo (negate (fromIntegral (abs acc) * sysh / 4.0) - accPos) (baseline - y + sysh / 8.0)
        setFontSize (noteWidth * 1.5)
        textPath (take (abs acc) (repeat 'b'))
        fill
