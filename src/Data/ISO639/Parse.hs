module Data.ISO639.Parse 
  ( parseAlpha3Bibliographic
  , parseAlpha3Terminologic
  , parseAlpha2
  ) where

import qualified Data.HashMap.Strict as Map
import Data.ISO639.Types (ISO639_2, alpha3_terminologic, alpha3_bibliographic, iso639_2s, alpha2)
import Data.Text(Text)

idx_a3b = foldr fld Map.empty iso639_2s
  where
  fld iso acc = Map.insert (alpha3_bibliographic iso) iso acc

idx_a3t = foldr fld Map.empty iso639_2s
  where
  fld iso acc = case alpha3_terminologic iso of {Nothing -> acc ; Just a3 -> Map.insert a3 iso acc}
       
idx_a2  = foldr fld Map.empty iso639_2s
  where
  fld iso acc = case alpha2 iso of {Nothing -> acc ; Just a3 -> Map.insert a3 iso acc}

parseAlpha3Bibliographic :: Text -> Maybe ISO639_2
parseAlpha3Bibliographic input = Map.lookup input idx_a3b
  
parseAlpha3Terminologic :: Text -> Maybe ISO639_2
parseAlpha3Terminologic input = Map.lookup input idx_a3t

parseAlpha2 :: Text -> Maybe ISO639_2
parseAlpha2 input = Map.lookup input idx_a2

