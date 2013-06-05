module Data.ISO639.Parse 
  ( parseAlpha3Bibliographic
  , parseAlpha3Terminologic
  , parseAlpha2
  ) where

import qualified Data.HashMap.Strict as Map
import Data.ISO639.Types (Language, alpha3_terminologic, alpha3_bibliographic, languages, alpha2)
import Data.Text(Text)

idx_a3b = foldr fld Map.empty languages
  where
  fld iso acc = Map.insert (alpha3_bibliographic iso) iso acc

idx_a3t = foldr fld Map.empty languages
  where
  fld iso acc = case alpha3_terminologic iso of {Nothing -> acc ; Just a3 -> Map.insert a3 iso acc}
       
idx_a2  = foldr fld Map.empty languages
  where
  fld iso acc = case alpha2 iso of {Nothing -> acc ; Just a3 -> Map.insert a3 iso acc}

parseAlpha3Bibliographic :: Text -> Maybe Language
parseAlpha3Bibliographic input = Map.lookup input idx_a3b
  
parseAlpha3Terminologic :: Text -> Maybe Language
parseAlpha3Terminologic input = Map.lookup input idx_a3t

parseAlpha2 :: Text -> Maybe Language
parseAlpha2 input = Map.lookup input idx_a2

