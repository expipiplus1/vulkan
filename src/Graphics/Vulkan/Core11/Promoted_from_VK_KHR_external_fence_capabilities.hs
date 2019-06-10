{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceFeatureFlagBits
  , pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , ExternalFenceFeatureFlagBitsKHR
  , ExternalFenceFeatureFlags
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceHandleTypeFlagBits
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , ExternalFenceHandleTypeFlagBitsKHR
  , ExternalFenceHandleTypeFlags
  , ExternalFenceHandleTypeFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , ExternalFenceProperties(..)
  , PhysicalDeviceExternalFenceInfo(..)
  , getPhysicalDeviceExternalFenceProperties
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits(..)
  , VkExternalFenceHandleTypeFlagBits(..)
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( vkGetPhysicalDeviceExternalFenceProperties
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
  ( pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )


-- No documentation found for TopLevel "ExternalFenceFeatureFlagBits"
type ExternalFenceFeatureFlagBits = VkExternalFenceFeatureFlagBits


{-# complete EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT, EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT :: ExternalFenceFeatureFlagBits #-}


-- No documentation found for Nested "ExternalFenceFeatureFlagBits" "EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT :: (a ~ ExternalFenceFeatureFlagBits) => a
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT


-- No documentation found for Nested "ExternalFenceFeatureFlagBits" "EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT :: (a ~ ExternalFenceFeatureFlagBits) => a
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "ExternalFenceFeatureFlagBitsKHR"
type ExternalFenceFeatureFlagBitsKHR = ExternalFenceFeatureFlagBits

-- No documentation found for TopLevel "ExternalFenceFeatureFlags"
type ExternalFenceFeatureFlags = ExternalFenceFeatureFlagBits

-- No documentation found for TopLevel "ExternalFenceFeatureFlagsKHR"
type ExternalFenceFeatureFlagsKHR = ExternalFenceFeatureFlags

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagBits"
type ExternalFenceHandleTypeFlagBits = VkExternalFenceHandleTypeFlagBits


{-# complete EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT, EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT, EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT, EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :: ExternalFenceHandleTypeFlagBits #-}


-- No documentation found for Nested "ExternalFenceHandleTypeFlagBits" "EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT


-- No documentation found for Nested "ExternalFenceHandleTypeFlagBits" "EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- No documentation found for Nested "ExternalFenceHandleTypeFlagBits" "EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- No documentation found for Nested "ExternalFenceHandleTypeFlagBits" "EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT"
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :: (a ~ ExternalFenceHandleTypeFlagBits) => a
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT = VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagBitsKHR"
type ExternalFenceHandleTypeFlagBitsKHR = ExternalFenceHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlags"
type ExternalFenceHandleTypeFlags = ExternalFenceHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagsKHR"
type ExternalFenceHandleTypeFlagsKHR = ExternalFenceHandleTypeFlags


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkExternalFenceProperties"
data ExternalFenceProperties = ExternalFenceProperties
  { -- No documentation found for Nested "ExternalFenceProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExternalFenceProperties" "exportFromImportedHandleTypes"
  exportFromImportedHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "ExternalFenceProperties" "compatibleHandleTypes"
  compatibleHandleTypes :: ExternalFenceHandleTypeFlags
  , -- No documentation found for Nested "ExternalFenceProperties" "externalFenceFeatures"
  externalFenceFeatures :: ExternalFenceFeatureFlags
  }
  deriving (Show, Eq)

instance Zero ExternalFenceProperties where
  zero = ExternalFenceProperties Nothing
                                 zero
                                 zero
                                 zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfo"
data PhysicalDeviceExternalFenceInfo = PhysicalDeviceExternalFenceInfo
  { -- No documentation found for Nested "PhysicalDeviceExternalFenceInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalFenceInfo" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceExternalFenceInfo where
  zero = PhysicalDeviceExternalFenceInfo Nothing
                                         zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalFenceProperties"
getPhysicalDeviceExternalFenceProperties :: PhysicalDevice ->  PhysicalDeviceExternalFenceInfo ->  IO (ExternalFenceProperties)
getPhysicalDeviceExternalFenceProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif
