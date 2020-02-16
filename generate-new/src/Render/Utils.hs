module Render.Utils
  where

import           Relude
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )

braceList :: Vector (Doc ()) -> Doc ()
braceList ds = if V.null ds
  then "{}"
  else align $ vsep $ (zipWith (<+>) ("{" : repeat ",") (toList ds)) <> ["}"]
