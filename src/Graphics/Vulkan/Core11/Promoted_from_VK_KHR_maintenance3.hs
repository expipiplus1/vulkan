{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , vkGetDescriptorSetLayoutSupport
  , VkPhysicalDeviceMaintenance3Properties(..)
  , VkDescriptorSetLayoutSupport(..)
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES = VkStructureType 1000168000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT = VkStructureType 1000168001
-- | vkGetDescriptorSetLayoutSupport - Query whether a descriptor set layout
-- can be created
--
-- = Parameters
--
-- -   @device@ is the logical device that would create the descriptor set
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'
--     structure specifying the state of the descriptor set layout object.
--
-- -   @pSupport@ points to a 'VkDescriptorSetLayoutSupport' structure in
--     which information about support for the descriptor set layout object
--     is returned.
--
-- = Description
--
-- Some implementations have limitations on what fits in a descriptor set
-- which are not easily expressible in terms of existing limits like
-- @maxDescriptorSet@*, for example if all descriptor types share a limited
-- space in memory but each descriptor is a different size or alignment.
-- This command returns information about whether a descriptor set
-- satisfies this limit. If the descriptor set layout satisfies the
-- 'VkPhysicalDeviceMaintenance3Properties'::@maxPerSetDescriptors@ limit,
-- this command is guaranteed to return @VK_TRUE@ in
-- 'VkDescriptorSetLayoutSupport'::@supported@. If the descriptor set
-- layout exceeds the
-- 'VkPhysicalDeviceMaintenance3Properties'::@maxPerSetDescriptors@ limit,
-- whether the descriptor set layout is supported is
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
-- This is a @VkDevice@ query rather than @VkPhysicalDevice@ because the
-- answer /may/ depend on enabled features.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDescriptorSetLayoutCreateInfo@ structure
--
-- -   @pSupport@ /must/ be a valid pointer to a
--     @VkDescriptorSetLayoutSupport@ structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo',
-- 'VkDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDescriptorSetLayoutSupport" vkGetDescriptorSetLayoutSupport :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
-- | VkPhysicalDeviceMaintenance3Properties - Structure describing descriptor
-- set properties
--
-- = Members
--
-- The members of the @VkPhysicalDeviceMaintenance3Properties@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @maxPerSetDescriptors@ is a maximum number of descriptors (summed
--     over all descriptor types) in a single descriptor set that is
--     guaranteed to satisfy any implementation-dependent constraints on
--     the size of a descriptor set itself. Applications /can/ query
--     whether a descriptor set that goes beyond this limit is supported
--     using 'vkGetDescriptorSetLayoutSupport'.
--
-- -   @maxMemoryAllocationSize@ is the maximum size of a memory allocation
--     that /can/ be created, even if there is more space available in the
--     heap.
--
-- If the @VkPhysicalDeviceMaintenance3Properties@ structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES@
--
-- = See Also
--
-- @VkDeviceSize@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceMaintenance3Properties = VkPhysicalDeviceMaintenance3Properties
  { -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "maxPerSetDescriptors"
  vkMaxPerSetDescriptors :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMaintenance3Properties" "maxMemoryAllocationSize"
  vkMaxMemoryAllocationSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMaintenance3Properties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMaintenance3Properties <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 16) (vkMaxPerSetDescriptors (poked :: VkPhysicalDeviceMaintenance3Properties))
                *> poke (ptr `plusPtr` 24) (vkMaxMemoryAllocationSize (poked :: VkPhysicalDeviceMaintenance3Properties))
-- | VkDescriptorSetLayoutSupport - Structure returning information about
-- whether a descriptor set layout can be supported
--
-- = Description
--
-- @supported@ is set to @VK_TRUE@ if the descriptor set /can/ be created,
-- or else is set to @VK_FALSE@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetVariableDescriptorCountLayoutSupportEXT'
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupportKHR'
data VkDescriptorSetLayoutSupport = VkDescriptorSetLayoutSupport
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @supported@ specifies whether the descriptor set layout /can/ be
  -- created.
  vkSupported :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutSupport where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutSupport <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutSupport))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutSupport))
                *> poke (ptr `plusPtr` 16) (vkSupported (poked :: VkDescriptorSetLayoutSupport))
