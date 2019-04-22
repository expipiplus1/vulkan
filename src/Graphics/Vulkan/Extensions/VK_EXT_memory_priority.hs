{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_memory_priority
  ( withCStructMemoryPriorityAllocateInfoEXT
  , fromCStructMemoryPriorityAllocateInfoEXT
  , MemoryPriorityAllocateInfoEXT(..)
  , withCStructPhysicalDeviceMemoryPriorityFeaturesEXT
  , fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT
  , PhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern EXT_MEMORY_PRIORITY_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  , pattern STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( VkMemoryPriorityAllocateInfoEXT(..)
  , VkPhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  )



-- | VkMemoryPriorityAllocateInfoEXT - Specify a memory allocation priority
--
-- = Description
--
-- Memory allocations with higher priority /may/ be more likely to stay in
-- device-local memory when the system is under memory pressure.
--
-- If this structure is not included, it is as if the @priority@ value were
-- @0.5@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data MemoryPriorityAllocateInfoEXT = MemoryPriorityAllocateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryPriorityAllocateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryPriorityAllocateInfoEXT" "priority"
  priority :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryPriorityAllocateInfoEXT' and
-- marshal a 'MemoryPriorityAllocateInfoEXT' into it. The 'VkMemoryPriorityAllocateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryPriorityAllocateInfoEXT :: MemoryPriorityAllocateInfoEXT -> (VkMemoryPriorityAllocateInfoEXT -> IO a) -> IO a
withCStructMemoryPriorityAllocateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryPriorityAllocateInfoEXT)) (\pPNext -> cont (VkMemoryPriorityAllocateInfoEXT VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT pPNext (priority (marshalled :: MemoryPriorityAllocateInfoEXT))))

-- | A function to read a 'VkMemoryPriorityAllocateInfoEXT' and all additional
-- structures in the pointer chain into a 'MemoryPriorityAllocateInfoEXT'.
fromCStructMemoryPriorityAllocateInfoEXT :: VkMemoryPriorityAllocateInfoEXT -> IO MemoryPriorityAllocateInfoEXT
fromCStructMemoryPriorityAllocateInfoEXT c = MemoryPriorityAllocateInfoEXT <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryPriorityAllocateInfoEXT)))
                                                                           <*> pure (vkPriority (c :: VkMemoryPriorityAllocateInfoEXT))

instance Zero MemoryPriorityAllocateInfoEXT where
  zero = MemoryPriorityAllocateInfoEXT Nothing
                                       zero



-- | VkPhysicalDeviceMemoryPriorityFeaturesEXT - Structure describing memory
-- priority features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority.VkPhysicalDeviceMemoryPriorityFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority.VkPhysicalDeviceMemoryPriorityFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority.VkPhysicalDeviceMemoryPriorityFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceMemoryPriorityFeaturesEXT = PhysicalDeviceMemoryPriorityFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryPriorityFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryPriorityFeaturesEXT" "memoryPriority"
  memoryPriority :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMemoryPriorityFeaturesEXT' and
-- marshal a 'PhysicalDeviceMemoryPriorityFeaturesEXT' into it. The 'VkPhysicalDeviceMemoryPriorityFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMemoryPriorityFeaturesEXT :: PhysicalDeviceMemoryPriorityFeaturesEXT -> (VkPhysicalDeviceMemoryPriorityFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceMemoryPriorityFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMemoryPriorityFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceMemoryPriorityFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT pPNext (boolToBool32 (memoryPriority (marshalled :: PhysicalDeviceMemoryPriorityFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceMemoryPriorityFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMemoryPriorityFeaturesEXT'.
fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT :: VkPhysicalDeviceMemoryPriorityFeaturesEXT -> IO PhysicalDeviceMemoryPriorityFeaturesEXT
fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT c = PhysicalDeviceMemoryPriorityFeaturesEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMemoryPriorityFeaturesEXT)))
                                                                                               <*> pure (bool32ToBool (vkMemoryPriority (c :: VkPhysicalDeviceMemoryPriorityFeaturesEXT)))

instance Zero PhysicalDeviceMemoryPriorityFeaturesEXT where
  zero = PhysicalDeviceMemoryPriorityFeaturesEXT Nothing
                                                 False


-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME"
pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME = VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_SPEC_VERSION"
pattern EXT_MEMORY_PRIORITY_SPEC_VERSION :: Integral a => a
pattern EXT_MEMORY_PRIORITY_SPEC_VERSION = VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
