{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceFeatureFlagBits
  , ExternalFenceFeatureFlagBitsKHR
  , ExternalFenceFeatureFlags
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceHandleTypeFlagBits
  , ExternalFenceHandleTypeFlagBitsKHR
  , ExternalFenceHandleTypeFlags
  , ExternalFenceHandleTypeFlagsKHR
  , withCStructExternalFenceProperties
  , fromCStructExternalFenceProperties
  , ExternalFenceProperties(..)
  , withCStructPhysicalDeviceExternalFenceInfo
  , fromCStructPhysicalDeviceExternalFenceInfo
  , PhysicalDeviceExternalFenceInfo(..)
  , getPhysicalDeviceExternalFenceProperties
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  ) where

import Control.Monad
  ( (<=<)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getPhysicalDeviceExternalFenceProperties
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits(..)
  , VkExternalFenceHandleTypeFlagBits(..)
  , VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ExternalFenceFeatureFlagBits"
type ExternalFenceFeatureFlagBits = VkExternalFenceFeatureFlagBits
-- No documentation found for TopLevel "ExternalFenceFeatureFlagBitsKHR"
type ExternalFenceFeatureFlagBitsKHR = ExternalFenceFeatureFlagBits
-- No documentation found for TopLevel "ExternalFenceFeatureFlags"
type ExternalFenceFeatureFlags = ExternalFenceFeatureFlagBits
-- No documentation found for TopLevel "ExternalFenceFeatureFlagsKHR"
type ExternalFenceFeatureFlagsKHR = ExternalFenceFeatureFlags
-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagBits"
type ExternalFenceHandleTypeFlagBits = VkExternalFenceHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagBitsKHR"
type ExternalFenceHandleTypeFlagBitsKHR = ExternalFenceHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalFenceHandleTypeFlags"
type ExternalFenceHandleTypeFlags = ExternalFenceHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagsKHR"
type ExternalFenceHandleTypeFlagsKHR = ExternalFenceHandleTypeFlags
-- No documentation found for TopLevel "ExternalFenceProperties"
data ExternalFenceProperties = ExternalFenceProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalFenceProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalFenceProperties" "exportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "ExternalFenceProperties" "compatibleHandleTypes"
  vkCompatibleHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "ExternalFenceProperties" "externalFenceFeatures"
  vkExternalFenceFeatures :: ExternalFenceFeatureFlags
  }
  deriving (Show, Eq)
withCStructExternalFenceProperties :: ExternalFenceProperties -> (VkExternalFenceProperties -> IO a) -> IO a
withCStructExternalFenceProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExternalFenceProperties)) (\pPNext -> cont (VkExternalFenceProperties VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES pPNext (vkExportFromImportedHandleTypes (from :: ExternalFenceProperties)) (vkCompatibleHandleTypes (from :: ExternalFenceProperties)) (vkExternalFenceFeatures (from :: ExternalFenceProperties))))
fromCStructExternalFenceProperties :: VkExternalFenceProperties -> IO ExternalFenceProperties
fromCStructExternalFenceProperties c = ExternalFenceProperties <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalFenceProperties)))
                                                               <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalFenceProperties))
                                                               <*> pure (vkCompatibleHandleTypes (c :: VkExternalFenceProperties))
                                                               <*> pure (vkExternalFenceFeatures (c :: VkExternalFenceProperties))
-- No documentation found for TopLevel "PhysicalDeviceExternalFenceInfo"
data PhysicalDeviceExternalFenceInfo = PhysicalDeviceExternalFenceInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceExternalFenceInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalFenceInfo" "handleType"
  vkHandleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceExternalFenceInfo :: PhysicalDeviceExternalFenceInfo -> (VkPhysicalDeviceExternalFenceInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalFenceInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceExternalFenceInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalFenceInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO pPNext (vkHandleType (from :: PhysicalDeviceExternalFenceInfo))))
fromCStructPhysicalDeviceExternalFenceInfo :: VkPhysicalDeviceExternalFenceInfo -> IO PhysicalDeviceExternalFenceInfo
fromCStructPhysicalDeviceExternalFenceInfo c = PhysicalDeviceExternalFenceInfo <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalFenceInfo)))
                                                                               <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalFenceInfo))

-- | Wrapper for vkGetPhysicalDeviceExternalFenceProperties
getPhysicalDeviceExternalFenceProperties :: PhysicalDevice ->  PhysicalDeviceExternalFenceInfo ->  IO (ExternalFenceProperties)
getPhysicalDeviceExternalFenceProperties = \(PhysicalDevice physicalDevice commandTable) -> \externalFenceInfo -> alloca (\pExternalFenceProperties -> (\a -> withCStructPhysicalDeviceExternalFenceInfo a . flip with) externalFenceInfo (\pExternalFenceInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceExternalFenceProperties commandTable physicalDevice pExternalFenceInfo pExternalFenceProperties *> ((fromCStructExternalFenceProperties <=< peek) pExternalFenceProperties)))
