{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_memory_budget
  ( withCStructPhysicalDeviceMemoryBudgetPropertiesEXT
  , fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT
  , PhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern EXT_MEMORY_BUDGET_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  )
import qualified Data.Vector.Generic
  ( convert
  )
import qualified Data.Vector.Generic.Sized
  ( convert
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
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
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Marshal.Utils
  ( padSized
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  )



-- | VkPhysicalDeviceMemoryBudgetPropertiesEXT - Structure specifying
-- physical device memory budget and usage
--
-- = Description
--
-- The values returned in this structure are not invariant. The
-- @heapBudget@ and @heapUsage@ values /must/ be zero for array elements
-- greater than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be non-zero for array elements less than
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap'::@size@ for
-- each heap.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceMemoryBudgetPropertiesEXT = PhysicalDeviceMemoryBudgetPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "heapBudget"
  heapBudget :: Vector DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "heapUsage"
  heapUsage :: Vector DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMemoryBudgetPropertiesEXT' and
-- marshal a 'PhysicalDeviceMemoryBudgetPropertiesEXT' into it. The 'VkPhysicalDeviceMemoryBudgetPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMemoryBudgetPropertiesEXT :: PhysicalDeviceMemoryBudgetPropertiesEXT -> (VkPhysicalDeviceMemoryBudgetPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceMemoryBudgetPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMemoryBudgetPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceMemoryBudgetPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT pPNext (Data.Vector.Generic.Sized.convert (padSized 0 (heapBudget (marshalled :: PhysicalDeviceMemoryBudgetPropertiesEXT)))) (Data.Vector.Generic.Sized.convert (padSized 0 (heapUsage (marshalled :: PhysicalDeviceMemoryBudgetPropertiesEXT))))))

-- | A function to read a 'VkPhysicalDeviceMemoryBudgetPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMemoryBudgetPropertiesEXT'.
fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT :: VkPhysicalDeviceMemoryBudgetPropertiesEXT -> IO PhysicalDeviceMemoryBudgetPropertiesEXT
fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT c = PhysicalDeviceMemoryBudgetPropertiesEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMemoryBudgetPropertiesEXT)))
                                                                                               <*> pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkHeapBudget (c :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))))
                                                                                               <*> pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkHeapUsage (c :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))))

instance Zero PhysicalDeviceMemoryBudgetPropertiesEXT where
  zero = PhysicalDeviceMemoryBudgetPropertiesEXT Nothing
                                                 Data.Vector.empty
                                                 Data.Vector.empty


-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_EXTENSION_NAME"
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME = VK_EXT_MEMORY_BUDGET_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_SPEC_VERSION"
pattern EXT_MEMORY_BUDGET_SPEC_VERSION :: Integral a => a
pattern EXT_MEMORY_BUDGET_SPEC_VERSION = VK_EXT_MEMORY_BUDGET_SPEC_VERSION
