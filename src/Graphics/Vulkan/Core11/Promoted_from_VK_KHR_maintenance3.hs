{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DescriptorSetLayoutSupport(..)
  , 
  PhysicalDeviceMaintenance3Properties(..)
  , getDescriptorSetLayoutSupport
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( vkGetDescriptorSetLayoutSupport
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSetLayoutCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
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
  ( pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorSetLayoutSupport"
data DescriptorSetLayoutSupport = DescriptorSetLayoutSupport
  { -- No documentation found for Nested "DescriptorSetLayoutSupport" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutSupport" "supported"
  supported :: Bool
  }
  deriving (Show, Eq)

instance Zero DescriptorSetLayoutSupport where
  zero = DescriptorSetLayoutSupport Nothing
                                    False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance3Properties"
data PhysicalDeviceMaintenance3Properties = PhysicalDeviceMaintenance3Properties
  { -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "maxPerSetDescriptors"
  maxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "maxMemoryAllocationSize"
  maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMaintenance3Properties where
  zero = PhysicalDeviceMaintenance3Properties Nothing
                                              zero
                                              zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetDescriptorSetLayoutSupport"
getDescriptorSetLayoutSupport :: Device ->  DescriptorSetLayoutCreateInfo ->  IO (DescriptorSetLayoutSupport)
getDescriptorSetLayoutSupport = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif
