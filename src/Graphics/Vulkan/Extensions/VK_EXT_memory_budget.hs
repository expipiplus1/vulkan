{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_memory_budget
  ( withCStructPhysicalDeviceMemoryBudgetPropertiesEXT
  , fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT
  , PhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION
  , pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  ) where

import Data.Vector
  ( Vector
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


import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceMemoryBudgetPropertiesEXT"
data PhysicalDeviceMemoryBudgetPropertiesEXT = PhysicalDeviceMemoryBudgetPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "heapBudget"
  vkHeapBudget :: Vector DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceMemoryBudgetPropertiesEXT" "heapUsage"
  vkHeapUsage :: Vector DeviceSize
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMemoryBudgetPropertiesEXT :: PhysicalDeviceMemoryBudgetPropertiesEXT -> (VkPhysicalDeviceMemoryBudgetPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceMemoryBudgetPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMemoryBudgetPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceMemoryBudgetPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT pPNext (Data.Vector.Generic.Sized.convert (padSized 0 (vkHeapBudget (from :: PhysicalDeviceMemoryBudgetPropertiesEXT)))) (Data.Vector.Generic.Sized.convert (padSized 0 (vkHeapUsage (from :: PhysicalDeviceMemoryBudgetPropertiesEXT))))))
fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT :: VkPhysicalDeviceMemoryBudgetPropertiesEXT -> IO PhysicalDeviceMemoryBudgetPropertiesEXT
fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT c = PhysicalDeviceMemoryBudgetPropertiesEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMemoryBudgetPropertiesEXT)))
                                                                                               <*> pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkHeapBudget (c :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))))
                                                                                               <*> pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkHeapUsage (c :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))))
