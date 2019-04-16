{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( withCStructDeviceQueueInfo2
  , fromCStructDeviceQueueInfo2
  , DeviceQueueInfo2(..)
  , withCStructPhysicalDeviceProtectedMemoryFeatures
  , fromCStructPhysicalDeviceProtectedMemoryFeatures
  , PhysicalDeviceProtectedMemoryFeatures(..)
  , withCStructPhysicalDeviceProtectedMemoryProperties
  , fromCStructPhysicalDeviceProtectedMemoryProperties
  , PhysicalDeviceProtectedMemoryProperties(..)
  , withCStructProtectedSubmitInfo
  , fromCStructProtectedSubmitInfo
  , ProtectedSubmitInfo(..)
  , getDeviceQueue2
  , pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern VK_QUEUE_PROTECTED_BIT
  , pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_BUFFER_CREATE_PROTECTED_BIT
  , pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
  ) where

import Data.Word
  ( Word32
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
  ( getDeviceQueue2
  )


import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkProtectedSubmitInfo(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.Device
  ( DeviceQueueCreateFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Queue(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_BUFFER_CREATE_PROTECTED_BIT
  , pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
  , pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_QUEUE_PROTECTED_BIT
  )


-- No documentation found for TopLevel "DeviceQueueInfo2"
data DeviceQueueInfo2 = DeviceQueueInfo2
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceQueueInfo2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueInfo2" "flags"
  vkFlags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "DeviceQueueInfo2" "queueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "DeviceQueueInfo2" "queueIndex"
  vkQueueIndex :: Word32
  }
  deriving (Show, Eq)
withCStructDeviceQueueInfo2 :: DeviceQueueInfo2 -> (VkDeviceQueueInfo2 -> IO a) -> IO a
withCStructDeviceQueueInfo2 from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceQueueInfo2)) (\pPNext -> cont (VkDeviceQueueInfo2 VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 pPNext (vkFlags (from :: DeviceQueueInfo2)) (vkQueueFamilyIndex (from :: DeviceQueueInfo2)) (vkQueueIndex (from :: DeviceQueueInfo2))))
fromCStructDeviceQueueInfo2 :: VkDeviceQueueInfo2 -> IO DeviceQueueInfo2
fromCStructDeviceQueueInfo2 c = DeviceQueueInfo2 <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceQueueInfo2)))
                                                 <*> pure (vkFlags (c :: VkDeviceQueueInfo2))
                                                 <*> pure (vkQueueFamilyIndex (c :: VkDeviceQueueInfo2))
                                                 <*> pure (vkQueueIndex (c :: VkDeviceQueueInfo2))
-- No documentation found for TopLevel "PhysicalDeviceProtectedMemoryFeatures"
data PhysicalDeviceProtectedMemoryFeatures = PhysicalDeviceProtectedMemoryFeatures
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceProtectedMemoryFeatures" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProtectedMemoryFeatures" "protectedMemory"
  vkProtectedMemory :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceProtectedMemoryFeatures :: PhysicalDeviceProtectedMemoryFeatures -> (VkPhysicalDeviceProtectedMemoryFeatures -> IO a) -> IO a
withCStructPhysicalDeviceProtectedMemoryFeatures from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceProtectedMemoryFeatures)) (\pPNext -> cont (VkPhysicalDeviceProtectedMemoryFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES pPNext (boolToBool32 (vkProtectedMemory (from :: PhysicalDeviceProtectedMemoryFeatures)))))
fromCStructPhysicalDeviceProtectedMemoryFeatures :: VkPhysicalDeviceProtectedMemoryFeatures -> IO PhysicalDeviceProtectedMemoryFeatures
fromCStructPhysicalDeviceProtectedMemoryFeatures c = PhysicalDeviceProtectedMemoryFeatures <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceProtectedMemoryFeatures)))
                                                                                           <*> pure (bool32ToBool (vkProtectedMemory (c :: VkPhysicalDeviceProtectedMemoryFeatures)))
-- No documentation found for TopLevel "PhysicalDeviceProtectedMemoryProperties"
data PhysicalDeviceProtectedMemoryProperties = PhysicalDeviceProtectedMemoryProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceProtectedMemoryProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProtectedMemoryProperties" "protectedNoFault"
  vkProtectedNoFault :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceProtectedMemoryProperties :: PhysicalDeviceProtectedMemoryProperties -> (VkPhysicalDeviceProtectedMemoryProperties -> IO a) -> IO a
withCStructPhysicalDeviceProtectedMemoryProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceProtectedMemoryProperties)) (\pPNext -> cont (VkPhysicalDeviceProtectedMemoryProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES pPNext (boolToBool32 (vkProtectedNoFault (from :: PhysicalDeviceProtectedMemoryProperties)))))
fromCStructPhysicalDeviceProtectedMemoryProperties :: VkPhysicalDeviceProtectedMemoryProperties -> IO PhysicalDeviceProtectedMemoryProperties
fromCStructPhysicalDeviceProtectedMemoryProperties c = PhysicalDeviceProtectedMemoryProperties <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceProtectedMemoryProperties)))
                                                                                               <*> pure (bool32ToBool (vkProtectedNoFault (c :: VkPhysicalDeviceProtectedMemoryProperties)))
-- No documentation found for TopLevel "ProtectedSubmitInfo"
data ProtectedSubmitInfo = ProtectedSubmitInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ProtectedSubmitInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ProtectedSubmitInfo" "protectedSubmit"
  vkProtectedSubmit :: Bool
  }
  deriving (Show, Eq)
withCStructProtectedSubmitInfo :: ProtectedSubmitInfo -> (VkProtectedSubmitInfo -> IO a) -> IO a
withCStructProtectedSubmitInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ProtectedSubmitInfo)) (\pPNext -> cont (VkProtectedSubmitInfo VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO pPNext (boolToBool32 (vkProtectedSubmit (from :: ProtectedSubmitInfo)))))
fromCStructProtectedSubmitInfo :: VkProtectedSubmitInfo -> IO ProtectedSubmitInfo
fromCStructProtectedSubmitInfo c = ProtectedSubmitInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkProtectedSubmitInfo)))
                                                       <*> pure (bool32ToBool (vkProtectedSubmit (c :: VkProtectedSubmitInfo)))

-- | Wrapper for vkGetDeviceQueue2
getDeviceQueue2 :: Device ->  DeviceQueueInfo2 ->  IO (Queue)
getDeviceQueue2 = \(Device device commandTable) -> \queueInfo -> alloca (\pQueue -> (\a -> withCStructDeviceQueueInfo2 a . flip with) queueInfo (\pQueueInfo -> Graphics.Vulkan.C.Dynamic.getDeviceQueue2 commandTable device pQueueInfo pQueue *> (flip Queue commandTable <$> peek pQueue)))
