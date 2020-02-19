module Render.Utils
  where

import           Relude
import           Data.Text.Prettyprint.Doc

parenList :: Foldable f => f (Doc ()) -> Doc ()
parenList = genericList "(" ")"

braceList :: Foldable f => f (Doc ()) -> Doc ()
braceList = genericList "{" "}"

genericList :: Foldable f => Doc () -> Doc () -> f (Doc ()) -> Doc ()
genericList l r ds = case toList ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> align $ vsep $ zipWith (<+>) (l : repeat ",") (toList ds) <> [r]

appList :: Foldable f => f (Doc ()) -> Doc ()
appList xs = align (vsep (zipWith (<+>) ("<$>" : repeat "<*>") (toList xs)))
