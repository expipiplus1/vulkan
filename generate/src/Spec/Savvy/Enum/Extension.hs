{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Enum.Extension
  ( EnumExtension(..)
  , specEnumExtensions
  , specBitmaskExtensions
  , extractEnumExtension
  , extractEnumExtensionAbsolute
  , extractBitmaskExtension
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Either.Validation
import           Data.Int
import           Data.Maybe
import qualified Data.MultiMap          as MultiMap
import           Data.MultiMap.Extra    ()
import           Data.Semigroup
import           Data.Text.Extra        as T
import           Data.Traversable
import           Data.Word
import           Prelude                hiding (Enum)
import qualified Spec.Extension         as P
import qualified Spec.Feature           as P
import qualified Spec.Feature           as PF
import           Spec.Savvy.Error
import qualified Spec.Spec              as P

data EnumExtension = EnumExtension
  { exName    :: Text
  , exValue   :: Either Int32 Word32
  , exComment :: Maybe Text
  }
  deriving (Show)

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
specRelativeExtensions P.Spec {..} =
  fmap MultiMap.fromList
    .  sequenceA
    $  [ extractEnumExtension Nothing ex
       | P.Feature {..}                              <- sFeatures
       , PF.ARequirement   P.FeatureRequirement {..} <- fElements
       , P.AnEnumExtension ex                        <- frInterfaces
       ]
    ++ [ extractEnumExtension (Just (fromIntegral extNumber)) ex
       | P.Extension {..} <- sExtensions
       , P.AnExtensionRequirement P.ExtensionRequirement {..} <- extElements
       , P.AnEnumExtension        ex <- erInterfaces
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
  in  MultiMap.fromList (extractEnumExtensionAbsolute <$> exs)

----------------------------------------------------------------
-- The extraction itself
----------------------------------------------------------------

extractEnumExtension
  :: Maybe Int
  -- ^ The extension number from the context, if any
  -> P.EnumExtension
  -- ^ The parsed extension
  -> Validation [SpecError] (Text, EnumExtension)
  -- ^ (What does this extend, the extension itself)
extractEnumExtension extNumber P.EnumExtension {..} =
  let exName = eexName
      value  = do
        extNumber' <- eexExtNumber <|> extNumber
        pure $ enumExtensionValue extNumber'
                                  eexOffset
                                  (fromMaybe P.Positive eexDirection)
      exComment = eexComment
  in  case value of
        Nothing -> Failure [EnumExtensionNumberMissing eexName]
        Just v ->
          let exValue = Left v in Success (eexExtends, EnumExtension {..})

extractEnumExtensionAbsolute
  :: P.EnumExtensionAbsolute
  -- ^ The parsed extension
  -> (Text, EnumExtension)
  -- ^ (What does this extend, the extension itself)
extractEnumExtensionAbsolute P.EnumExtensionAbsolute {..} =
  let exName    = eexaName
      exValue   = Left $ fromIntegral eexaValue
      exComment = eexaComment
  in  (eexaExtends, EnumExtension {..})

extractBitmaskExtension
  :: P.BitmaskExtension
  -- ^ The parsed extension
  -> (Text, EnumExtension)
  -- ^ (What does this extend, the extension itself)
extractBitmaskExtension P.BitmaskExtension {..} =
  let exName    = bmxName
      exValue   = Right $ 1 `shiftL` bmxBitPos
      exComment = bmxComment
  in  (bmxExtends, EnumExtension {..})

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
  :: P.Spec -> MultiMap.MultiMap Text EnumExtension
specBitmaskExtensions P.Spec{..} =
  let exs :: [P.BitmaskExtension]
      exs =
        [ ex
          | P.Feature {..} <- sFeatures
          , PF.ARequirement           P.FeatureRequirement {..} <- fElements
          , P.ABitmaskExtension ex <- frInterfaces
          ]
          ++ [ ex
             | P.Extension {..} <- sExtensions
             , P.AnExtensionRequirement  P.ExtensionRequirement {..} <-
               extElements
             , P.ABitmaskExtension ex <- erInterfaces
             ]
  in  MultiMap.fromList (extractBitmaskExtension <$> exs)
