module Data.SetPool
    ( SetPool, KeyMap
    , filterSingers
    ) where

import Data.Music.Tonal
import Data.Maybe
import Control.Monad
import qualified Data.Map as M

type SetPool = M.Map String KeyMap
type KeyMap = M.Map String Pitch

filterSingers :: [String] -> SetPool -> [(String, String, Pitch)]
filterSingers singers pool = mapMaybe (listToMaybe . filterSinger singers) (M.toList pool)

filterSinger :: [String] -> (String, KeyMap) -> [(String, String, Pitch)]
filterSinger singers (song, keymap) =
    fmap (\(a,b) -> (song,a,b)) $ (mapMaybe (matchSinger $ M.toList keymap) singers)
                               ++ (maybeToList $ matchSinger (M.toList keymap) "all")

matchSinger :: [(String, Pitch)] -> String -> Maybe (String, Pitch)
matchSinger db singer = fmap (\x -> (singer, x)) $ lookup singer db
