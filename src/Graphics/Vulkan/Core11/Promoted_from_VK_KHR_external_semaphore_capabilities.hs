{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreFeatureFlagBits
  , ExternalSemaphoreFeatureFlagBitsKHR
  , ExternalSemaphoreFeatureFlags
  , ExternalSemaphoreFeatureFlagsKHR
  , ExternalSemaphoreHandleTypeFlagBits
  , ExternalSemaphoreHandleTypeFlagBitsKHR
  , ExternalSemaphoreHandleTypeFlags
  , ExternalSemaphoreHandleTypeFlagsKHR
  , withCStructExternalSemaphoreProperties
  , fromCStructExternalSemaphoreProperties
  , ExternalSemaphoreProperties(..)
  , withCStructPhysicalDeviceExternalSemaphoreInfo
  , fromCStructPhysicalDeviceExternalSemaphoreInfo
  , PhysicalDeviceExternalSemaphoreInfo(..)
  , getPhysicalDeviceExternalSemaphoreProperties
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
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
  ( getPhysicalDeviceExternalSemaphoreProperties
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreFeatureFlagBits(..)
  , VkExternalSemaphoreHandleTypeFlagBits(..)
  , VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagBits"
type ExternalSemaphoreFeatureFlagBits = VkExternalSemaphoreFeatureFlagBits
-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagBitsKHR"
type ExternalSemaphoreFeatureFlagBitsKHR = ExternalSemaphoreFeatureFlagBits
-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlags"
type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits
-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagsKHR"
type ExternalSemaphoreFeatureFlagsKHR = ExternalSemaphoreFeatureFlags
-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagBits"
type ExternalSemaphoreHandleTypeFlagBits = VkExternalSemaphoreHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagBitsKHR"
type ExternalSemaphoreHandleTypeFlagBitsKHR = ExternalSemaphoreHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlags"
type ExternalSemaphoreHandleTypeFlags = ExternalSemaphoreHandleTypeFlagBits
-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagsKHR"
type ExternalSemaphoreHandleTypeFlagsKHR = ExternalSemaphoreHandleTypeFlags
-- No documentation found for TopLevel "ExternalSemaphoreProperties"
data ExternalSemaphoreProperties = ExternalSemaphoreProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "ExternalSemaphoreProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "exportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "compatibleHandleTypes"
  vkCompatibleHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "externalSemaphoreFeatures"
  vkExternalSemaphoreFeatures :: ExternalSemaphoreFeatureFlags
  }
  deriving (Show, Eq)
withCStructExternalSemaphoreProperties :: ExternalSemaphoreProperties -> (VkExternalSemaphoreProperties -> IO a) -> IO a
withCStructExternalSemaphoreProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExternalSemaphoreProperties)) (\pPNext -> cont (VkExternalSemaphoreProperties VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES pPNext (vkExportFromImportedHandleTypes (from :: ExternalSemaphoreProperties)) (vkCompatibleHandleTypes (from :: ExternalSemaphoreProperties)) (vkExternalSemaphoreFeatures (from :: ExternalSemaphoreProperties))))
fromCStructExternalSemaphoreProperties :: VkExternalSemaphoreProperties -> IO ExternalSemaphoreProperties
fromCStructExternalSemaphoreProperties c = ExternalSemaphoreProperties <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExternalSemaphoreProperties)))
                                                                       <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalSemaphoreProperties))
                                                                       <*> pure (vkCompatibleHandleTypes (c :: VkExternalSemaphoreProperties))
                                                                       <*> pure (vkExternalSemaphoreFeatures (c :: VkExternalSemaphoreProperties))
-- No documentation found for TopLevel "PhysicalDeviceExternalSemaphoreInfo"
data PhysicalDeviceExternalSemaphoreInfo = PhysicalDeviceExternalSemaphoreInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceExternalSemaphoreInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalSemaphoreInfo" "handleType"
  vkHandleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceExternalSemaphoreInfo :: PhysicalDeviceExternalSemaphoreInfo -> (VkPhysicalDeviceExternalSemaphoreInfo -> IO a) -> IO a
withCStructPhysicalDeviceExternalSemaphoreInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceExternalSemaphoreInfo)) (\pPNext -> cont (VkPhysicalDeviceExternalSemaphoreInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO pPNext (vkHandleType (from :: PhysicalDeviceExternalSemaphoreInfo))))
fromCStructPhysicalDeviceExternalSemaphoreInfo :: VkPhysicalDeviceExternalSemaphoreInfo -> IO PhysicalDeviceExternalSemaphoreInfo
fromCStructPhysicalDeviceExternalSemaphoreInfo c = PhysicalDeviceExternalSemaphoreInfo <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExternalSemaphoreInfo)))
                                                                                       <*> pure (vkHandleType (c :: VkPhysicalDeviceExternalSemaphoreInfo))

-- | Wrapper for 'vkGetPhysicalDeviceExternalSemaphoreProperties'
getPhysicalDeviceExternalSemaphoreProperties :: PhysicalDevice ->  PhysicalDeviceExternalSemaphoreInfo ->  IO ( ExternalSemaphoreProperties )
getPhysicalDeviceExternalSemaphoreProperties = \(PhysicalDevice physicalDevice commandTable) -> \externalSemaphoreInfo -> alloca (\pExternalSemaphoreProperties -> (\a -> withCStructPhysicalDeviceExternalSemaphoreInfo a . flip with) externalSemaphoreInfo (\pExternalSemaphoreInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceExternalSemaphoreProperties commandTable physicalDevice pExternalSemaphoreInfo pExternalSemaphoreProperties *> ((fromCStructExternalSemaphoreProperties <=< peek) pExternalSemaphoreProperties)))
