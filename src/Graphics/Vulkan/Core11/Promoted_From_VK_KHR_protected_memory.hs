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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkProtectedSubmitInfo(..)
  , vkGetDeviceQueue2
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



-- | VkDeviceQueueInfo2 - Structure specifying the parameters used for device
-- queue creation
--
-- = Description
--
-- The queue returned by
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2'
-- /must/ have the same @flags@ value from this structure as that used at
-- device creation time in a
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo' instance. If
-- no matching @flags@ were specified at device creation time then @pQueue@
-- will return 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2'
data DeviceQueueInfo2 = DeviceQueueInfo2
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceQueueInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueInfo2" "flags"
  flags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "DeviceQueueInfo2" "queueFamilyIndex"
  queueFamilyIndex :: Word32
  , -- No documentation found for Nested "DeviceQueueInfo2" "queueIndex"
  queueIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceQueueInfo2' and
-- marshal a 'DeviceQueueInfo2' into it. The 'VkDeviceQueueInfo2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceQueueInfo2 :: DeviceQueueInfo2 -> (VkDeviceQueueInfo2 -> IO a) -> IO a
withCStructDeviceQueueInfo2 marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceQueueInfo2)) (\pPNext -> cont (VkDeviceQueueInfo2 VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 pPNext (flags (marshalled :: DeviceQueueInfo2)) (queueFamilyIndex (marshalled :: DeviceQueueInfo2)) (queueIndex (marshalled :: DeviceQueueInfo2))))

-- | A function to read a 'VkDeviceQueueInfo2' and all additional
-- structures in the pointer chain into a 'DeviceQueueInfo2'.
fromCStructDeviceQueueInfo2 :: VkDeviceQueueInfo2 -> IO DeviceQueueInfo2
fromCStructDeviceQueueInfo2 c = DeviceQueueInfo2 <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceQueueInfo2)))
                                                 <*> pure (vkFlags (c :: VkDeviceQueueInfo2))
                                                 <*> pure (vkQueueFamilyIndex (c :: VkDeviceQueueInfo2))
                                                 <*> pure (vkQueueIndex (c :: VkDeviceQueueInfo2))

instance Zero DeviceQueueInfo2 where
  zero = DeviceQueueInfo2 Nothing
                          zero
                          zero
                          zero



-- | VkPhysicalDeviceProtectedMemoryFeatures - Structure describing protected
-- memory features that can be supported by an implementation
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryFeatures'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceProtectedMemoryFeatures = PhysicalDeviceProtectedMemoryFeatures
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceProtectedMemoryFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProtectedMemoryFeatures" "protectedMemory"
  protectedMemory :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceProtectedMemoryFeatures' and
-- marshal a 'PhysicalDeviceProtectedMemoryFeatures' into it. The 'VkPhysicalDeviceProtectedMemoryFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceProtectedMemoryFeatures :: PhysicalDeviceProtectedMemoryFeatures -> (VkPhysicalDeviceProtectedMemoryFeatures -> IO a) -> IO a
withCStructPhysicalDeviceProtectedMemoryFeatures marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceProtectedMemoryFeatures)) (\pPNext -> cont (VkPhysicalDeviceProtectedMemoryFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES pPNext (boolToBool32 (protectedMemory (marshalled :: PhysicalDeviceProtectedMemoryFeatures)))))

-- | A function to read a 'VkPhysicalDeviceProtectedMemoryFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceProtectedMemoryFeatures'.
fromCStructPhysicalDeviceProtectedMemoryFeatures :: VkPhysicalDeviceProtectedMemoryFeatures -> IO PhysicalDeviceProtectedMemoryFeatures
fromCStructPhysicalDeviceProtectedMemoryFeatures c = PhysicalDeviceProtectedMemoryFeatures <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceProtectedMemoryFeatures)))
                                                                                           <*> pure (bool32ToBool (vkProtectedMemory (c :: VkPhysicalDeviceProtectedMemoryFeatures)))

instance Zero PhysicalDeviceProtectedMemoryFeatures where
  zero = PhysicalDeviceProtectedMemoryFeatures Nothing
                                               False



-- | VkPhysicalDeviceProtectedMemoryProperties - Structure describing
-- protected memory properties that can be supported by an implementation
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryProperties'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with a value indicating the implementation-dependent
-- behavior.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceProtectedMemoryProperties = PhysicalDeviceProtectedMemoryProperties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceProtectedMemoryProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProtectedMemoryProperties" "protectedNoFault"
  protectedNoFault :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceProtectedMemoryProperties' and
-- marshal a 'PhysicalDeviceProtectedMemoryProperties' into it. The 'VkPhysicalDeviceProtectedMemoryProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceProtectedMemoryProperties :: PhysicalDeviceProtectedMemoryProperties -> (VkPhysicalDeviceProtectedMemoryProperties -> IO a) -> IO a
withCStructPhysicalDeviceProtectedMemoryProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceProtectedMemoryProperties)) (\pPNext -> cont (VkPhysicalDeviceProtectedMemoryProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES pPNext (boolToBool32 (protectedNoFault (marshalled :: PhysicalDeviceProtectedMemoryProperties)))))

-- | A function to read a 'VkPhysicalDeviceProtectedMemoryProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceProtectedMemoryProperties'.
fromCStructPhysicalDeviceProtectedMemoryProperties :: VkPhysicalDeviceProtectedMemoryProperties -> IO PhysicalDeviceProtectedMemoryProperties
fromCStructPhysicalDeviceProtectedMemoryProperties c = PhysicalDeviceProtectedMemoryProperties <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceProtectedMemoryProperties)))
                                                                                               <*> pure (bool32ToBool (vkProtectedNoFault (c :: VkPhysicalDeviceProtectedMemoryProperties)))

instance Zero PhysicalDeviceProtectedMemoryProperties where
  zero = PhysicalDeviceProtectedMemoryProperties Nothing
                                                 False



-- | VkProtectedSubmitInfo - Structure indicating whether the submission is
-- protected
--
-- == Valid Usage
--
-- -   If the protected memory feature is not enabled, @protectedSubmit@
--     /must/ not be 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
--
-- -   If @protectedSubmit@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     then each element of the @pCommandBuffers@ array /must/ be a
--     protected command buffer.
--
-- -   If @protectedSubmit@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE',
--     then each element of the @pCommandBuffers@ array /must/ be an
--     unprotected command buffer.
--
-- -   If the 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@pNext@ chain
--     does not include a
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkProtectedSubmitInfo'
--     structure, then each element of the command buffer of the
--     @pCommandBuffers@ array /must/ be an unprotected command buffer.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ProtectedSubmitInfo = ProtectedSubmitInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ProtectedSubmitInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ProtectedSubmitInfo" "protectedSubmit"
  protectedSubmit :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkProtectedSubmitInfo' and
-- marshal a 'ProtectedSubmitInfo' into it. The 'VkProtectedSubmitInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructProtectedSubmitInfo :: ProtectedSubmitInfo -> (VkProtectedSubmitInfo -> IO a) -> IO a
withCStructProtectedSubmitInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ProtectedSubmitInfo)) (\pPNext -> cont (VkProtectedSubmitInfo VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO pPNext (boolToBool32 (protectedSubmit (marshalled :: ProtectedSubmitInfo)))))

-- | A function to read a 'VkProtectedSubmitInfo' and all additional
-- structures in the pointer chain into a 'ProtectedSubmitInfo'.
fromCStructProtectedSubmitInfo :: VkProtectedSubmitInfo -> IO ProtectedSubmitInfo
fromCStructProtectedSubmitInfo c = ProtectedSubmitInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkProtectedSubmitInfo)))
                                                       <*> pure (bool32ToBool (vkProtectedSubmit (c :: VkProtectedSubmitInfo)))

instance Zero ProtectedSubmitInfo where
  zero = ProtectedSubmitInfo Nothing
                             False



-- | vkGetDeviceQueue2 - Get a queue handle from a device
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the queue.
--
-- -   @pQueueInfo@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2'
--     structure, describing the parameters used to create the device
--     queue.
--
-- -   @pQueue@ is a pointer to a 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
--     object that will be filled with the handle for the requested queue.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2',
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
getDeviceQueue2 :: Device ->  DeviceQueueInfo2 ->  IO (Queue)
getDeviceQueue2 = \(Device device' commandTable) -> \queueInfo' -> alloca (\pQueue' -> (\marshalled -> withCStructDeviceQueueInfo2 marshalled . flip with) queueInfo' (\pQueueInfo' -> vkGetDeviceQueue2 commandTable device' pQueueInfo' pQueue' *> (flip Queue commandTable <$> peek pQueue')))
