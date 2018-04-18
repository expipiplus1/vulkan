{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Savvy.Extension
  ( Extension(..)
  , P.ExtensionSupport(..)
  , specExtensions
  ) where

import           Data.Either.Validation
import           Data.Text              (Text)
import           Data.Traversable
import qualified Spec.Extension         as P
import           Spec.Savvy.Error
import           Spec.Savvy.Feature
import qualified Spec.Spec              as P

data Extension = Extension
  { extName         :: Text
  , extRequirements :: [Requirement]
  , extSupported    :: P.ExtensionSupport
  , extPlatform     :: Maybe Text
  }
  deriving (Show)

specExtensions :: P.Spec -> Validation [SpecError] [Extension]
specExtensions P.Spec {..} = for sExtensions $ \P.Extension {..} -> do
  let reqs = [ r | P.AnExtensionRequirement r <- extElements ]
  extRequirements <- traverse (extractRequirement (fromIntegral extNumber)) reqs
  pure Extension {..}

extractRequirement
  :: Int -> P.ExtensionRequirement -> Validation [SpecError] Requirement
extractRequirement extensionNumber P.ExtensionRequirement {..} = do
  let rFeature       = erFeature
      rComment       = erComment
      rRequiredNames = concatMap interfaceRequiredNames erInterfaces
      rEnumNames     = extractEnumNames erInterfaces
      rCommandNames  = extractCommandNames erInterfaces
      rConstants     = extractConstants erInterfaces
      rEnumAliases   = extractEnumAliases erInterfaces
  rEnumExtensions <- extractEnumExtensions (Just extensionNumber)
                                           erFeature
                                           erInterfaces
  pure Requirement {..}

