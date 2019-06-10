{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  BindBufferMemoryDeviceGroupInfo(..)
  , 
  BindImageMemoryDeviceGroupInfo(..)
#endif
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfo"
data BindBufferMemoryDeviceGroupInfo = BindBufferMemoryDeviceGroupInfo
  { -- No documentation found for Nested "BindBufferMemoryDeviceGroupInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindBufferMemoryDeviceGroupInfo" "pDeviceIndices"
  deviceIndices :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero BindBufferMemoryDeviceGroupInfo where
  zero = BindBufferMemoryDeviceGroupInfo Nothing
                                         mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfo"
data BindImageMemoryDeviceGroupInfo = BindImageMemoryDeviceGroupInfo
  { -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pDeviceIndices"
  deviceIndices :: Vector Word32
  , -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pSplitInstanceBindRegions"
  splitInstanceBindRegions :: Vector Rect2D
  }
  deriving (Show, Eq)

instance Zero BindImageMemoryDeviceGroupInfo where
  zero = BindImageMemoryDeviceGroupInfo Nothing
                                        mempty
                                        mempty

#endif
