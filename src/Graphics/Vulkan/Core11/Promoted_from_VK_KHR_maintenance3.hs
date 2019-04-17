{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( withCStructDescriptorSetLayoutSupport
  , fromCStructDescriptorSetLayoutSupport
  , DescriptorSetLayoutSupport(..)
  , withCStructPhysicalDeviceMaintenance3Properties
  , fromCStructPhysicalDeviceMaintenance3Properties
  , PhysicalDeviceMaintenance3Properties(..)
  , getDescriptorSetLayoutSupport
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  ) where

import Control.Monad
  ( (<=<)
  )
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
  ( getDescriptorSetLayoutSupport
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSetLayoutCreateInfo(..)
  , withCStructDescriptorSetLayoutCreateInfo
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "DescriptorSetLayoutSupport"
data DescriptorSetLayoutSupport = DescriptorSetLayoutSupport
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorSetLayoutSupport" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutSupport" "supported"
  vkSupported :: Bool
  }
  deriving (Show, Eq)
withCStructDescriptorSetLayoutSupport :: DescriptorSetLayoutSupport -> (VkDescriptorSetLayoutSupport -> IO a) -> IO a
withCStructDescriptorSetLayoutSupport from cont = maybeWith withSomeVkStruct (vkPNext (from :: DescriptorSetLayoutSupport)) (\pPNext -> cont (VkDescriptorSetLayoutSupport VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT pPNext (boolToBool32 (vkSupported (from :: DescriptorSetLayoutSupport)))))
fromCStructDescriptorSetLayoutSupport :: VkDescriptorSetLayoutSupport -> IO DescriptorSetLayoutSupport
fromCStructDescriptorSetLayoutSupport c = DescriptorSetLayoutSupport <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetLayoutSupport)))
                                                                     <*> pure (bool32ToBool (vkSupported (c :: VkDescriptorSetLayoutSupport)))
instance Zero DescriptorSetLayoutSupport where
  zero = DescriptorSetLayoutSupport Nothing
                                    False
-- No documentation found for TopLevel "PhysicalDeviceMaintenance3Properties"
data PhysicalDeviceMaintenance3Properties = PhysicalDeviceMaintenance3Properties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "maxPerSetDescriptors"
  vkMaxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "maxMemoryAllocationSize"
  vkMaxMemoryAllocationSize :: DeviceSize
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMaintenance3Properties :: PhysicalDeviceMaintenance3Properties -> (VkPhysicalDeviceMaintenance3Properties -> IO a) -> IO a
withCStructPhysicalDeviceMaintenance3Properties from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMaintenance3Properties)) (\pPNext -> cont (VkPhysicalDeviceMaintenance3Properties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES pPNext (vkMaxPerSetDescriptors (from :: PhysicalDeviceMaintenance3Properties)) (vkMaxMemoryAllocationSize (from :: PhysicalDeviceMaintenance3Properties))))
fromCStructPhysicalDeviceMaintenance3Properties :: VkPhysicalDeviceMaintenance3Properties -> IO PhysicalDeviceMaintenance3Properties
fromCStructPhysicalDeviceMaintenance3Properties c = PhysicalDeviceMaintenance3Properties <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMaintenance3Properties)))
                                                                                         <*> pure (vkMaxPerSetDescriptors (c :: VkPhysicalDeviceMaintenance3Properties))
                                                                                         <*> pure (vkMaxMemoryAllocationSize (c :: VkPhysicalDeviceMaintenance3Properties))
instance Zero PhysicalDeviceMaintenance3Properties where
  zero = PhysicalDeviceMaintenance3Properties Nothing
                                              zero
                                              zero

-- | Wrapper for 'vkGetDescriptorSetLayoutSupport'
getDescriptorSetLayoutSupport :: Device ->  DescriptorSetLayoutCreateInfo ->  IO (DescriptorSetLayoutSupport)
getDescriptorSetLayoutSupport = \(Device device commandTable) -> \createInfo -> alloca (\pSupport -> (\a -> withCStructDescriptorSetLayoutCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.getDescriptorSetLayoutSupport commandTable device pCreateInfo pSupport *> ((fromCStructDescriptorSetLayoutSupport <=< peek) pSupport)))
