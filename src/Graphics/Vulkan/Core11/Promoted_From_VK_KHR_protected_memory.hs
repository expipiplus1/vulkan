{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DeviceQueueInfo2(..)
  , 
  PhysicalDeviceProtectedMemoryFeatures(..)
  , PhysicalDeviceProtectedMemoryProperties(..)
  , ProtectedSubmitInfo(..)
#endif
  , getDeviceQueue2
  , pattern STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern QUEUE_PROTECTED_BIT
  , pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , pattern MEMORY_PROPERTY_PROTECTED_BIT
  , pattern BUFFER_CREATE_PROTECTED_BIT
  , pattern IMAGE_CREATE_PROTECTED_BIT
  , pattern COMMAND_POOL_CREATE_PROTECTED_BIT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( vkGetDeviceQueue2
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Device
  ( DeviceQueueCreateFlags
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Queue(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Buffer
  ( pattern BUFFER_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.Core10.CommandPool
  ( pattern COMMAND_POOL_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  )
import Graphics.Vulkan.Core10.Device
  ( pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_PROTECTED_BIT
  , pattern MEMORY_PROPERTY_PROTECTED_BIT
  , pattern QUEUE_PROTECTED_BIT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceQueueInfo2"
data DeviceQueueInfo2 = DeviceQueueInfo2
  { -- No documentation found for Nested "DeviceQueueInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueInfo2" "flags"
  flags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "DeviceQueueInfo2" "queueFamilyIndex"
  queueFamilyIndex :: Word32
  , -- No documentation found for Nested "DeviceQueueInfo2" "queueIndex"
  queueIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero DeviceQueueInfo2 where
  zero = DeviceQueueInfo2 Nothing
                          zero
                          zero
                          zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceProtectedMemoryFeatures"
data PhysicalDeviceProtectedMemoryFeatures = PhysicalDeviceProtectedMemoryFeatures
  { -- No documentation found for Nested "PhysicalDeviceProtectedMemoryFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProtectedMemoryFeatures" "protectedMemory"
  protectedMemory :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceProtectedMemoryFeatures where
  zero = PhysicalDeviceProtectedMemoryFeatures Nothing
                                               False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceProtectedMemoryProperties"
data PhysicalDeviceProtectedMemoryProperties = PhysicalDeviceProtectedMemoryProperties
  { -- No documentation found for Nested "PhysicalDeviceProtectedMemoryProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProtectedMemoryProperties" "protectedNoFault"
  protectedNoFault :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceProtectedMemoryProperties where
  zero = PhysicalDeviceProtectedMemoryProperties Nothing
                                                 False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkProtectedSubmitInfo"
data ProtectedSubmitInfo = ProtectedSubmitInfo
  { -- No documentation found for Nested "ProtectedSubmitInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ProtectedSubmitInfo" "protectedSubmit"
  protectedSubmit :: Bool
  }
  deriving (Show, Eq)

instance Zero ProtectedSubmitInfo where
  zero = ProtectedSubmitInfo Nothing
                             False

#endif


-- No documentation found for TopLevel "vkGetDeviceQueue2"
getDeviceQueue2 :: Device ->  DeviceQueueInfo2 ->  IO (Queue)
getDeviceQueue2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
