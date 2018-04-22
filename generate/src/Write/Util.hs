{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Write.Util
  ( intercalatePrepend
  , emptyLineSep
  , vcatPara
  , separatedSections
  , document
  , Documentee(..)
  , separatedWithGuards
  , guarded
  ) where

import           Data.Bifunctor
import           Data.Function
import           Data.Functor.Extra
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text.Extra           as T
import           Data.Text.Prettyprint.Doc

import           Documentation
import           Documentation.Haddock

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

-- Return a documentation rendering if possible, otherwise ""
document :: (Documentee -> Maybe Haddock) -> Documentee -> Doc ()
document getDoc n = case getDoc n of
  Nothing          -> "-- No documentation found for" <+> pretty (T.tShow n)
  Just (Haddock h) -> case T.lines h of
    [] -> "-- Empty Documentation Found for" <+> pretty (T.tShow n)
    x : xs ->
      vcat (("-- |" <> pretty (space x)) : (("--" <>) . pretty . space <$> xs))
    where
      space = \case
        "" -> ""
        x  -> " " <> x

separatedWithGuards
  :: Text
  -- ^ Separator
  -> [(Doc (), Maybe Text)]
  -- ^ Things to separate with optional guards
  -> Doc ()
separatedWithGuards sep things =
  let prefixedThings = case things of
        []   -> []
        x:xs -> x : (first sepPrefix <$> xs)
  in case mergeGuards prefixedThings of
    []       -> mempty
    (d : ds) -> vcatIndents $ concat ((uncurry (flip guardedLines) d) : (sepThings ds))
    where
      sepThings ds = ds <&> \(d, g) -> guardedLines g d
      sepPrefix = case sep of
        "" -> ("" <>)
        s  -> (pretty s <+>)

mergeGuards :: [(Doc (), Maybe Text)]
            -> [(Doc (), Maybe Text)]
mergeGuards xs =
  let groups :: [NonEmpty ((Doc (), Maybe Text))]
      groups = groupBy (sameGuard `on` snd) xs
      sameGuard (Just x) (Just y) = x == y
      sameGuard _ _               = False
      ungroups :: NonEmpty (Doc (), Maybe Text) -> (Doc (), Maybe Text)
      ungroups group =
        let (ds, g:|gs) = NE.unzip group
        in (vcat (NE.toList ds), g)
  in ungroups <$> groups

guardedLines :: Maybe Text -> Doc () -> [(Maybe Int, Doc ())]
guardedLines = \case
  Nothing -> \d -> [(Nothing, d)]
  Just g  -> \d ->
    [ (Just (-1000), "#if defined(" <> pretty g <> ")")
    , (Nothing      , d)
    , (Just (-1000), "#endif")
    ]

guarded :: Maybe Text -> Doc () -> Doc ()
guarded = \case
  Nothing -> id
  Just g  -> \d ->
    indent minBound (line <> "#if defined(" <> pretty g <> ")")
      <> line <> d
      <> indent minBound (line <> "#endif")

-- | Workaround for https://github.com/quchen/prettyprinter/issues/57
vcatIndents :: [(Maybe Int, Doc ())] -> Doc ()
vcatIndents = \case
  []                -> mempty
  (Nothing, d) : ds -> hcat $ (d : (addLine <$> ds))
  ds                -> hcat $ (addLine <$> ds)
  where
    addLine = \case
      (Just i , d) -> indent i (line <> d)
      (Nothing, d) -> line <> d
