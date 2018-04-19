{-# LANGUAGE LambdaCase #-}

module Write.Util
  ( intercalatePrepend
  , emptyLineSep
  , vcatPara
  , separatedSections
  ) where

import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

-- | 'intercalatePrepend d (x:xs)' will prepend with a space d to xs
intercalatePrepend :: Doc () -> [Doc ()] -> [Doc ()]
intercalatePrepend _ []     = []
intercalatePrepend i (m:ms) = m : ((i <+>) <$> ms)

emptyLineSep :: Foldable f => f (Doc a) -> Doc a
emptyLineSep = concatWith (\a b -> a <> line <> line <> b)

-- | 'vcat' but insers a leading and trailing newline if the list is non-empty
vcatPara :: [Doc a] -> Doc a
vcatPara = \case
  [] -> mempty
  xs -> line <> vcat xs <> line

separatedSections
  :: Text
  -- ^ Separator
  -> [(Maybe (Doc ()), [Doc ()])]
     -- A list of sections with an optional heading and a list of elements
     -- If a section has no elements it is omitted
  -> Doc ()
separatedSections separator sections = vcat $ case nonEmptySections of
  []     -> []
  x : xs -> firstSection x ++ concat (subsequentSection <$> xs)
  where
    nonEmptySections = mapMaybe (traverse nonEmpty) sections
    firstSection     = \case
      (Just header, x :| xs) ->
        header
          : indent (T.length separator + 1) x
          : ((pretty separator <+>) <$> xs)
      (Nothing, x :| xs) -> x : ((pretty separator <+>) <$> xs)
    subsequentSection = \case
      (Just header, x :| xs) ->
        (pretty separator <+> header)
          : indent (T.length separator + 1) x
          : ((pretty separator <+>) <$> xs)
      (Nothing, x :| xs) -> ((pretty separator <+>) <$> (x : xs))
