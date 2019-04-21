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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  , vkGetDescriptorSetLayoutSupport
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



-- | VkDescriptorSetLayoutSupport - Structure returning information about
-- whether a descriptor set layout can be supported
--
-- = Description
--
-- @supported@ is set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if the
-- descriptor set /can/ be created, or else is set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- Unresolved directive in VkDescriptorSetLayoutSupport.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetLayoutSupport.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport'
data DescriptorSetLayoutSupport = DescriptorSetLayoutSupport
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorSetLayoutSupport" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutSupport" "supported"
  supported :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetLayoutSupport' and
-- marshal a 'DescriptorSetLayoutSupport' into it. The 'VkDescriptorSetLayoutSupport' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetLayoutSupport :: DescriptorSetLayoutSupport -> (VkDescriptorSetLayoutSupport -> IO a) -> IO a
withCStructDescriptorSetLayoutSupport marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DescriptorSetLayoutSupport)) (\pPNext -> cont (VkDescriptorSetLayoutSupport VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT pPNext (boolToBool32 (supported (marshalled :: DescriptorSetLayoutSupport)))))

-- | A function to read a 'VkDescriptorSetLayoutSupport' and all additional
-- structures in the pointer chain into a 'DescriptorSetLayoutSupport'.
fromCStructDescriptorSetLayoutSupport :: VkDescriptorSetLayoutSupport -> IO DescriptorSetLayoutSupport
fromCStructDescriptorSetLayoutSupport c = DescriptorSetLayoutSupport <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetLayoutSupport)))
                                                                     <*> pure (bool32ToBool (vkSupported (c :: VkDescriptorSetLayoutSupport)))

instance Zero DescriptorSetLayoutSupport where
  zero = DescriptorSetLayoutSupport Nothing
                                    False



-- | VkPhysicalDeviceMaintenance3Properties - Structure describing descriptor
-- set properties
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceMaintenance3Properties.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMaintenance3Properties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceMaintenance3Properties = PhysicalDeviceMaintenance3Properties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "maxPerSetDescriptors"
  maxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMaintenance3Properties" "maxMemoryAllocationSize"
  maxMemoryAllocationSize :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMaintenance3Properties' and
-- marshal a 'PhysicalDeviceMaintenance3Properties' into it. The 'VkPhysicalDeviceMaintenance3Properties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMaintenance3Properties :: PhysicalDeviceMaintenance3Properties -> (VkPhysicalDeviceMaintenance3Properties -> IO a) -> IO a
withCStructPhysicalDeviceMaintenance3Properties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMaintenance3Properties)) (\pPNext -> cont (VkPhysicalDeviceMaintenance3Properties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES pPNext (maxPerSetDescriptors (marshalled :: PhysicalDeviceMaintenance3Properties)) (maxMemoryAllocationSize (marshalled :: PhysicalDeviceMaintenance3Properties))))

-- | A function to read a 'VkPhysicalDeviceMaintenance3Properties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMaintenance3Properties'.
fromCStructPhysicalDeviceMaintenance3Properties :: VkPhysicalDeviceMaintenance3Properties -> IO PhysicalDeviceMaintenance3Properties
fromCStructPhysicalDeviceMaintenance3Properties c = PhysicalDeviceMaintenance3Properties <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMaintenance3Properties)))
                                                                                         <*> pure (vkMaxPerSetDescriptors (c :: VkPhysicalDeviceMaintenance3Properties))
                                                                                         <*> pure (vkMaxMemoryAllocationSize (c :: VkPhysicalDeviceMaintenance3Properties))

instance Zero PhysicalDeviceMaintenance3Properties where
  zero = PhysicalDeviceMaintenance3Properties Nothing
                                              zero
                                              zero



-- | vkGetDescriptorSetLayoutSupport - Query whether a descriptor set layout
-- can be created
--
-- = Parameters
--
-- -   @device@ is the logical device that would create the descriptor set
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'
--     structure specifying the state of the descriptor set layout object.
--
-- -   @pSupport@ points to a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkDescriptorSetLayoutSupport'
--     structure in which information about support for the descriptor set
--     layout object is returned.
--
-- = Description
--
-- Some implementations have limitations on what fits in a descriptor set
-- which are not easily expressible in terms of existing limits like
-- @maxDescriptorSet@*, for example if all descriptor types share a limited
-- space in memory but each descriptor is a different size or alignment.
-- This command returns information about whether a descriptor set
-- satisfies this limit. If the descriptor set layout satisfies the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties'::@maxPerSetDescriptors@
-- limit, this command is guaranteed to return
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkDescriptorSetLayoutSupport'::@supported@.
-- If the descriptor set layout exceeds the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties'::@maxPerSetDescriptors@
-- limit, whether the descriptor set layout is supported is
-- implementation-dependent and /may/ depend on whether the descriptor
-- sizes and alignments cause the layout to exceed an internal limit.
--
-- This command does not consider other limits such as
-- @maxPerStageDescriptor@*, and so a descriptor set layout that is
-- supported according to this command /must/ still satisfy the pipeline
-- layout limits such as @maxPerStageDescriptor@* in order to be used in a
-- pipeline layout.
--
-- __Note__
--
-- This is a 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' query
-- rather than
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice' because
-- the answer /may/ depend on enabled features.
--
-- Unresolved directive in vkGetDescriptorSetLayoutSupport.txt -
-- include::{generated}\/validity\/protos\/vkGetDescriptorSetLayoutSupport.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
getDescriptorSetLayoutSupport :: Device ->  DescriptorSetLayoutCreateInfo ->  IO (DescriptorSetLayoutSupport)
getDescriptorSetLayoutSupport = \(Device device' commandTable) -> \createInfo' -> alloca (\pSupport' -> (\marshalled -> withCStructDescriptorSetLayoutCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkGetDescriptorSetLayoutSupport commandTable device' pCreateInfo' pSupport' *> ((fromCStructDescriptorSetLayoutSupport <=< peek) pSupport')))
