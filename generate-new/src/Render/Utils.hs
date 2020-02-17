module Render.Utils
  where

import           Relude
import           Data.Text.Prettyprint.Doc
import           Data.Vector.Extra       hiding ( zipWith, toList )

parenList :: Vector (Doc ()) -> Doc ()
parenList = genericList "(" ")"

braceList :: Vector (Doc ()) -> Doc ()
braceList = genericList "{" "}"

genericList :: Doc () -> Doc () -> Vector (Doc ()) -> Doc ()
genericList l r = \case
  Empty -> l <> r
  Singleton d -> l <> d <> r
  ds -> align $ vsep $ zipWith (<+>) (l : repeat ",") (toList ds) <> [r]
