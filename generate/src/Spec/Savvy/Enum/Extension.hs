{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Enum.Extension
  ( EnumExtension(..)
  , specEnumExtensions
  , specBitmaskExtensions
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Either.Validation
import           Data.Int
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.MultiMap          as MultiMap
import           Data.MultiMap.Extra    ()
import           Data.Semigroup
import qualified Data.Set               as Set
import           Data.Text.Extra        as T
import           Data.Traversable
import           Data.Word
import           Prelude                hiding (Enum)
import qualified Spec.Bitmask           as P
import qualified Spec.Enum              as P
import qualified Spec.Extension         as P
import qualified Spec.Feature           as P
import qualified Spec.Feature           as PF
import           Spec.Savvy.Error
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data EnumExtension = EnumExtension
  { exName    :: Text
  , exValue   :: Either Int32 Word32
  , exComment :: Maybe Text
  }

----------------------------------------------------------------
-- Enums
----------------------------------------------------------------

-- | Get all the numeric enum extensions mentioned in a spec
specEnumExtensions
  :: P.Spec -> ([SpecError], MultiMap.MultiMap Text EnumExtension)
specEnumExtensions spec =
  let absoluteExtensions = specAbsoluteExtensions spec
  in  case specRelativeExtensions spec of
        Failure es -> (es, absoluteExtensions)
        Success es -> ([], absoluteExtensions <> es)

specRelativeExtensions
  :: P.Spec -> Validation [SpecError] (MultiMap.MultiMap Text EnumExtension)
specRelativeExtensions P.Spec {..} = do
  features <-
    fmap MultiMap.fromList
    . sequenceA
    $ [ case value of
          Nothing -> Failure [EnumExtensionNumberMissing "eexName"]
          Just v ->
            let exValue = Left v in Success (eexExtends, EnumExtension {..})
      | P.Feature {..}                              <- sFeatures
      , PF.ARequirement   P.FeatureRequirement {..} <- fElements
      , P.AnEnumExtension P.EnumExtension {..}      <- frInterfaces
      , let exName = eexName
            value  = do
              extNumber <- eexExtNumber
              pure $ enumExtensionValue extNumber
                                        eexOffset
                                        (fromMaybe P.Positive eexDirection)
            exComment = eexComment
      ]
  pure $ features <> MultiMap.fromList
    [ (eexExtends, EnumExtension {..})
    | P.Extension {..} <- sExtensions
    , P.AnExtensionRequirement P.ExtensionRequirement {..} <- extElements
    , P.AnEnumExtension        P.EnumExtension {..} <- erInterfaces
    , let exName  = eexName
          exValue = Left $ enumExtensionValue
            (fromMaybe (fromIntegral extNumber) eexExtNumber)
            eexOffset
            (fromMaybe P.Positive eexDirection)
          exComment = eexComment
    ]

specAbsoluteExtensions :: P.Spec -> MultiMap.MultiMap Text EnumExtension
specAbsoluteExtensions P.Spec {..} =
  let exs :: [P.EnumExtensionAbsolute]
      exs =
        [ ex
          | P.Feature {..} <- sFeatures
          , PF.ARequirement           P.FeatureRequirement {..} <- fElements
          , P.AnEnumExtensionAbsolute ex <- frInterfaces
          ]
          ++ [ ex
             | P.Extension {..} <- sExtensions
             , P.AnExtensionRequirement  P.ExtensionRequirement {..} <-
               extElements
             , P.AnEnumExtensionAbsolute ex <- erInterfaces
             ]
  in  MultiMap.fromList
        [ (eexaExtends, EnumExtension {..})
        | P.EnumExtensionAbsolute {..} <- exs
        , let exName    = eexaName
              exValue   = Left $ fromIntegral eexaValue
              exComment = eexaComment
        ]

-- | Implementation from the spec's generator.py 'enumToValue'
enumExtensionValue
  :: Int
  -- ^ The extension number
  -> Int
  -- ^ Offset
  -> P.Direction
  -- ^ The offset direction
  -> Int32
  -- ^ The enum value
enumExtensionValue extNumber offset direction =
  let extBlockSize = 1000
      extBase = 1000000000
      n = extBase + pred (fromIntegral extNumber) * extBlockSize + fromIntegral offset
  in case direction of
       P.Positive -> n
       P.Negative -> negate n

----------------------------------------------------------------
-- Bitmasks
----------------------------------------------------------------

-- | Get all the numeric bitmask extensions mentioned in a spec
specBitmaskExtensions
  :: P.Spec -> ([SpecError], MultiMap.MultiMap Text EnumExtension)
specBitmaskExtensions _ = ([], mempty)
