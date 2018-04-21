{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Documentation.Haddock
  ( Haddock(..)
  , DocumenteeLocation(..)
  , documentationToHaddock
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Bifunctor
import           Data.Default
import           Data.Either.Combinators
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                    (Text)
import qualified Data.Text.Extra              as T
import           Documentation.RunAsciiDoctor
import           Prelude                      hiding (rem)
import           Say
import           System.Environment
import           Text.Pandoc

import           Documentation

newtype Haddock = Haddock { unHaddock :: Text }
  deriving (Show)

data DocumenteeLocation
  = Unknown
  | ThisModule
  | OtherModule Text

documentationToHaddock
  :: (Text -> DocumenteeLocation)
  -- ^ Find which module a documentee lives in
  -> Documentation
  -- ^ The documentation to render
  -> Either Text Haddock
documentationToHaddock getModule Documentation {..} =
  let writerOptions = def
  in  bimap T.tShow Haddock $ runPure
        (writeHaddock
          writerOptions
          (prepareForHaddock . fixLinks getModule $ dDocumentation)
        )

prepareForHaddock :: Pandoc -> Pandoc
prepareForHaddock =
  topDown haddock801 . topDown removeEmptySections . topDown fixupBlock
  where
    haddock801 :: [Block] -> [Block]
    haddock801 = foldr go []
      where
        dummy = Para [Str "'"]
        go :: Block -> [Block] -> [Block]
        go h@(Header{}) (t@(Table{}) : bs) = h : dummy : t : bs
        go b            bs                 = b : bs

    removeEmptySections :: [Block] -> [Block]
    removeEmptySections = foldr go []
      where
        go :: Block -> [Block] -> [Block]
        go (Header n1 _ _) (h@(Header n2 _ _) : bs) | n1 <= n2 = h : bs
        go b               bs                       = b : bs

    fixupBlock :: Block -> Block
    fixupBlock = \case
      -- Remove idents from headers
      Header n (_, cs, kvs) is -> Header n ("", cs, kvs) is

      -- Change definition lists to bullets
      DefinitionList ds | all (null . fst) ds, all ((== 1) . length . snd) ds ->
        BulletList (head . snd <$> ds)

      b -> b

fixLinks :: (Text -> DocumenteeLocation) -> Pandoc -> Pandoc
fixLinks findDocs = topDown fixInlines
  where
    fixInlines = \case
      Link ("", [], []) [Str name] (tag, "") | tag == "#" <> name ->
        case findDocs (T.pack name) of
          Unknown    -> Code ("", [], []) name
          ThisModule -> RawInline "haddock" ("'" <> name <> "'")
          OtherModule m ->
            RawInline "haddock" ("'" <> T.unpack m <> "." <> name <> "'")
      -- Because of https://github.com/haskell/haddock/issues/802 the best we
      -- can do is link to the spec
      Link attrs t (tag, title)
        | Just fragment <- T.dropPrefix "#" (T.pack tag) -> Link
          attrs
          t
          (externalSpecHTML <> "#" <> T.unpack fragment, title)
      Link attrs t (tag, title)
        | Just fragment <- T.dropPrefix "{html_spec_relative}#" (T.pack tag) -> Link
          attrs
          t
          (externalSpecHTML <> "#" <> T.unpack fragment, title)
      i -> i

f = Link ("", [], [])
         [Str "depth", Space, Str "bias", Space, Str "clamping"]
         ("{html_spec_relative}#features-features-depthBiasClamp", "")

externalSpecHTML :: String
externalSpecHTML
  = "https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html"
