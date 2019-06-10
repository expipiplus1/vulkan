{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreFeatureFlagBits
  , pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , ExternalSemaphoreFeatureFlagBitsKHR
  , ExternalSemaphoreFeatureFlags
  , ExternalSemaphoreFeatureFlagsKHR
  , ExternalSemaphoreHandleTypeFlagBits
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , ExternalSemaphoreHandleTypeFlagBitsKHR
  , ExternalSemaphoreHandleTypeFlags
  , ExternalSemaphoreHandleTypeFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , ExternalSemaphoreProperties(..)
  , PhysicalDeviceExternalSemaphoreInfo(..)
  , getPhysicalDeviceExternalSemaphoreProperties
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreFeatureFlagBits(..)
  , VkExternalSemaphoreHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( vkGetPhysicalDeviceExternalSemaphoreProperties
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )


-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagBits"
type ExternalSemaphoreFeatureFlagBits = VkExternalSemaphoreFeatureFlagBits


{-# complete EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT, EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT :: ExternalSemaphoreFeatureFlagBits #-}


-- No documentation found for Nested "ExternalSemaphoreFeatureFlagBits" "EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT :: (a ~ ExternalSemaphoreFeatureFlagBits) => a
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT = VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT


-- No documentation found for Nested "ExternalSemaphoreFeatureFlagBits" "EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT :: (a ~ ExternalSemaphoreFeatureFlagBits) => a
pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT = VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagBitsKHR"
type ExternalSemaphoreFeatureFlagBitsKHR = ExternalSemaphoreFeatureFlagBits

-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlags"
type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits

-- No documentation found for TopLevel "ExternalSemaphoreFeatureFlagsKHR"
type ExternalSemaphoreFeatureFlagsKHR = ExternalSemaphoreFeatureFlags

-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagBits"
type ExternalSemaphoreHandleTypeFlagBits = VkExternalSemaphoreHandleTypeFlagBits


{-# complete EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT, EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT :: ExternalSemaphoreHandleTypeFlagBits #-}


-- No documentation found for Nested "ExternalSemaphoreHandleTypeFlagBits" "EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT


-- No documentation found for Nested "ExternalSemaphoreHandleTypeFlagBits" "EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- No documentation found for Nested "ExternalSemaphoreHandleTypeFlagBits" "EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- No documentation found for Nested "ExternalSemaphoreHandleTypeFlagBits" "EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT


-- No documentation found for Nested "ExternalSemaphoreHandleTypeFlagBits" "EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT :: (a ~ ExternalSemaphoreHandleTypeFlagBits) => a
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT

-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagBitsKHR"
type ExternalSemaphoreHandleTypeFlagBitsKHR = ExternalSemaphoreHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlags"
type ExternalSemaphoreHandleTypeFlags = ExternalSemaphoreHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalSemaphoreHandleTypeFlagsKHR"
type ExternalSemaphoreHandleTypeFlagsKHR = ExternalSemaphoreHandleTypeFlags


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalSemaphoreProperties"
data ExternalSemaphoreProperties = ExternalSemaphoreProperties
  { -- No documentation found for Nested "ExternalSemaphoreProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- No documentation found for Nested "ExternalSemaphoreProperties" "externalSemaphoreFeatures"
  externalSemaphoreFeatures :: ExternalSemaphoreFeatureFlags
  }
  deriving (Show, Eq)

instance Zero ExternalSemaphoreProperties where
  zero = ExternalSemaphoreProperties Nothing
                                     zero
                                     zero
                                     zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceExternalSemaphoreInfo"
data PhysicalDeviceExternalSemaphoreInfo = PhysicalDeviceExternalSemaphoreInfo
  { -- No documentation found for Nested "PhysicalDeviceExternalSemaphoreInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalSemaphoreInfo" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceExternalSemaphoreInfo where
  zero = PhysicalDeviceExternalSemaphoreInfo Nothing
                                             zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalSemaphoreProperties"
getPhysicalDeviceExternalSemaphoreProperties :: PhysicalDevice ->  PhysicalDeviceExternalSemaphoreInfo ->  IO (ExternalSemaphoreProperties)
getPhysicalDeviceExternalSemaphoreProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif
