{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( withCStructExternalImageFormatPropertiesNV
  , fromCStructExternalImageFormatPropertiesNV
  , ExternalImageFormatPropertiesNV(..)
  , ExternalMemoryFeatureFlagBitsNV
  , ExternalMemoryFeatureFlagsNV
  , ExternalMemoryHandleTypeFlagBitsNV
  , ExternalMemoryHandleTypeFlagsNV
  , getPhysicalDeviceExternalImageFormatPropertiesNV
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getPhysicalDeviceExternalImageFormatPropertiesNV
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryFeatureFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagBitsNV(..)
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageFormatProperties(..)
  , PhysicalDevice(..)
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , fromCStructImageFormatProperties
  , withCStructImageFormatProperties
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  )


-- No documentation found for TopLevel "ExternalImageFormatPropertiesNV"
data ExternalImageFormatPropertiesNV = ExternalImageFormatPropertiesNV
  { -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "imageFormatProperties"
  vkImageFormatProperties :: ImageFormatProperties
  , -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "externalMemoryFeatures"
  vkExternalMemoryFeatures :: ExternalMemoryFeatureFlagsNV
  , -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "exportFromImportedHandleTypes"
  vkExportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  , -- No documentation found for Nested "ExternalImageFormatPropertiesNV" "compatibleHandleTypes"
  vkCompatibleHandleTypes :: ExternalMemoryHandleTypeFlagsNV
  }
  deriving (Show, Eq)
withCStructExternalImageFormatPropertiesNV :: ExternalImageFormatPropertiesNV -> (VkExternalImageFormatPropertiesNV -> IO a) -> IO a
withCStructExternalImageFormatPropertiesNV from cont = withCStructImageFormatProperties (vkImageFormatProperties (from :: ExternalImageFormatPropertiesNV)) (\imageFormatProperties -> cont (VkExternalImageFormatPropertiesNV imageFormatProperties (vkExternalMemoryFeatures (from :: ExternalImageFormatPropertiesNV)) (vkExportFromImportedHandleTypes (from :: ExternalImageFormatPropertiesNV)) (vkCompatibleHandleTypes (from :: ExternalImageFormatPropertiesNV))))
fromCStructExternalImageFormatPropertiesNV :: VkExternalImageFormatPropertiesNV -> IO ExternalImageFormatPropertiesNV
fromCStructExternalImageFormatPropertiesNV c = ExternalImageFormatPropertiesNV <$> (fromCStructImageFormatProperties (vkImageFormatProperties (c :: VkExternalImageFormatPropertiesNV)))
                                                                               <*> pure (vkExternalMemoryFeatures (c :: VkExternalImageFormatPropertiesNV))
                                                                               <*> pure (vkExportFromImportedHandleTypes (c :: VkExternalImageFormatPropertiesNV))
                                                                               <*> pure (vkCompatibleHandleTypes (c :: VkExternalImageFormatPropertiesNV))
-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsNV"
type ExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV
-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsNV"
type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV
-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBitsNV"
type ExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV
-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsNV"
type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV

-- | Wrapper for vkGetPhysicalDeviceExternalImageFormatPropertiesNV
getPhysicalDeviceExternalImageFormatPropertiesNV :: PhysicalDevice ->  Format ->  ImageType ->  ImageTiling ->  ImageUsageFlags ->  ImageCreateFlags ->  ExternalMemoryHandleTypeFlagsNV ->  IO (ExternalImageFormatPropertiesNV)
getPhysicalDeviceExternalImageFormatPropertiesNV = \(PhysicalDevice physicalDevice commandTable) -> \format -> \type' -> \tiling -> \usage -> \flags -> \externalHandleType -> alloca (\pExternalImageFormatProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceExternalImageFormatPropertiesNV commandTable physicalDevice format type' tiling usage flags externalHandleType pExternalImageFormatProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructExternalImageFormatPropertiesNV <=< peek) pExternalImageFormatProperties)))
