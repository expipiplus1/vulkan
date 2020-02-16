module Render.Utils
  where

import           Relude
import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )

parenList :: Vector (Doc ()) -> Doc ()
parenList = genericList "(" ")"

braceList :: Vector (Doc ()) -> Doc ()
braceList = genericList "{" "}"

genericList :: Doc () -> Doc () -> Vector (Doc ()) -> Doc ()
genericList l r ds = if V.null ds
  then l <> r
  else align $ vsep $ (zipWith (<+>) (l : repeat ",") (toList ds)) <> [r]
