{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DeviceGroupDeviceCreateInfo(..)
  , 
  PhysicalDeviceGroupProperties(..)
  , getNumPhysicalDeviceGroups
  , enumeratePhysicalDeviceGroups
  , enumerateAllPhysicalDeviceGroups
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( vkEnumeratePhysicalDeviceGroups
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Instance(..)
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
  ( pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupDeviceCreateInfo"
data DeviceGroupDeviceCreateInfo = DeviceGroupDeviceCreateInfo
  { -- No documentation found for Nested "DeviceGroupDeviceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupDeviceCreateInfo" "pPhysicalDevices"
  physicalDevices :: Vector PhysicalDevice
  }
  deriving (Show, Eq)

instance Zero DeviceGroupDeviceCreateInfo where
  zero = DeviceGroupDeviceCreateInfo Nothing
                                     mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceGroupProperties"
data PhysicalDeviceGroupProperties = PhysicalDeviceGroupProperties
  { -- No documentation found for Nested "PhysicalDeviceGroupProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceGroupProperties" "physicalDevices"
  physicalDevices :: Vector PhysicalDevice
  , -- No documentation found for Nested "PhysicalDeviceGroupProperties" "subsetAllocation"
  subsetAllocation :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceGroupProperties where
  zero = PhysicalDeviceGroupProperties Nothing
                                       mempty
                                       False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceGroups"
getNumPhysicalDeviceGroups :: Instance ->  IO (VkResult, Word32)
getNumPhysicalDeviceGroups = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceGroups"
enumeratePhysicalDeviceGroups :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDeviceGroupProperties)
enumeratePhysicalDeviceGroups = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'enumeratePhysicalDeviceGroups'.
enumerateAllPhysicalDeviceGroups :: Instance ->  IO (Vector PhysicalDeviceGroupProperties)
enumerateAllPhysicalDeviceGroups instance' =
  snd <$> getNumPhysicalDeviceGroups instance'
    >>= \num -> snd <$> enumeratePhysicalDeviceGroups instance' num

#endif
