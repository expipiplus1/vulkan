{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_features
  ( ValidationFeatureDisableEXT
  , ValidationFeatureEnableEXT
  , withCStructValidationFeaturesEXT
  , fromCStructValidationFeaturesEXT
  , ValidationFeaturesEXT(..)
  , pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
  , pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeatureDisableEXT(..)
  , VkValidationFeatureEnableEXT(..)
  , VkValidationFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
  )


-- No documentation found for TopLevel "ValidationFeatureDisableEXT"
type ValidationFeatureDisableEXT = VkValidationFeatureDisableEXT
-- No documentation found for TopLevel "ValidationFeatureEnableEXT"
type ValidationFeatureEnableEXT = VkValidationFeatureEnableEXT
-- No documentation found for TopLevel "ValidationFeaturesEXT"
data ValidationFeaturesEXT = ValidationFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ValidationFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ValidationFeaturesEXT" "pEnabledValidationFeatures"
  vkPEnabledValidationFeatures :: Vector ValidationFeatureEnableEXT
  -- Length valued member elided
  , -- No documentation found for Nested "ValidationFeaturesEXT" "pDisabledValidationFeatures"
  vkPDisabledValidationFeatures :: Vector ValidationFeatureDisableEXT
  }
  deriving (Show, Eq)
withCStructValidationFeaturesEXT :: ValidationFeaturesEXT -> (VkValidationFeaturesEXT -> IO a) -> IO a
withCStructValidationFeaturesEXT from cont = withVec (&) (vkPDisabledValidationFeatures (from :: ValidationFeaturesEXT)) (\pDisabledValidationFeatures -> withVec (&) (vkPEnabledValidationFeatures (from :: ValidationFeaturesEXT)) (\pEnabledValidationFeatures -> maybeWith withSomeVkStruct (vkPNext (from :: ValidationFeaturesEXT)) (\pPNext -> cont (VkValidationFeaturesEXT VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT pPNext (fromIntegral (Data.Vector.length (vkPEnabledValidationFeatures (from :: ValidationFeaturesEXT)))) pEnabledValidationFeatures (fromIntegral (Data.Vector.length (vkPDisabledValidationFeatures (from :: ValidationFeaturesEXT)))) pDisabledValidationFeatures))))
fromCStructValidationFeaturesEXT :: VkValidationFeaturesEXT -> IO ValidationFeaturesEXT
fromCStructValidationFeaturesEXT c = ValidationFeaturesEXT <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkValidationFeaturesEXT)))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkEnabledValidationFeatureCount (c :: VkValidationFeaturesEXT))) (peekElemOff (vkPEnabledValidationFeatures (c :: VkValidationFeaturesEXT))))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkDisabledValidationFeatureCount (c :: VkValidationFeaturesEXT))) (peekElemOff (vkPDisabledValidationFeatures (c :: VkValidationFeaturesEXT))))
instance Zero ValidationFeaturesEXT where
  zero = ValidationFeaturesEXT Nothing
                               Data.Vector.empty
                               Data.Vector.empty
