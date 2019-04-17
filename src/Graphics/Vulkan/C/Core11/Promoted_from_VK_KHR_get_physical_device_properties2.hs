{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkSparseImageFormatProperties2(..)
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceFeatures2
#endif
  , FN_vkGetPhysicalDeviceFeatures2
  , PFN_vkGetPhysicalDeviceFeatures2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceFormatProperties2
#endif
  , FN_vkGetPhysicalDeviceFormatProperties2
  , PFN_vkGetPhysicalDeviceFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceImageFormatProperties2
#endif
  , FN_vkGetPhysicalDeviceImageFormatProperties2
  , PFN_vkGetPhysicalDeviceImageFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceMemoryProperties2
#endif
  , FN_vkGetPhysicalDeviceMemoryProperties2
  , PFN_vkGetPhysicalDeviceMemoryProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceProperties2
#endif
  , FN_vkGetPhysicalDeviceProperties2
  , PFN_vkGetPhysicalDeviceProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceQueueFamilyProperties2
#endif
  , FN_vkGetPhysicalDeviceQueueFamilyProperties2
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceSparseImageFormatProperties2
#endif
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties2
  , PFN_vkGetPhysicalDeviceSparseImageFormatProperties2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkQueueFamilyProperties(..)
  , VkSampleCountFlagBits(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkSparseImageFormatProperties(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkFormatProperties2 - Structure specifying image format properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceFormatProperties2'
data VkFormatProperties2 = VkFormatProperties2
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @formatProperties@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties'
  -- describing features supported by the requested format.
  vkFormatProperties :: VkFormatProperties
  }
  deriving (Eq, Show)

instance Storable VkFormatProperties2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFormatProperties2 <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFormatProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFormatProperties2))
                *> poke (ptr `plusPtr` 16) (vkFormatProperties (poked :: VkFormatProperties2))

instance Zero VkFormatProperties2 where
  zero = VkFormatProperties2 zero
                             zero
                             zero
-- | VkImageFormatProperties2 - Structure specifying an image format
-- properties
--
-- = Description
--
-- If the combination of parameters to
-- @vkGetPhysicalDeviceImageFormatProperties2@ is not supported by the
-- implementation for use in
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage', then all members of
-- @imageFormatProperties@ will be filled with zero.
--
-- __Note__
--
-- Filling @imageFormatProperties@ with zero for unsupported formats is an
-- exception to the usual rule that output structures have undefined
-- contents on error. This exception was unintentional, but is preserved
-- for backwards compatibility. This exeption only applies to
-- @imageFormatProperties@, not @sType@, @pNext@, or any structures chained
-- from @pNext@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionImageFormatProperties'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceImageFormatProperties2'
data VkImageFormatProperties2 = VkImageFormatProperties2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure. The
  -- @pNext@ chain of @VkImageFormatProperties2@ is used to allow the
  -- specification of additional capabilities to be returned from
  -- @vkGetPhysicalDeviceImageFormatProperties2@.
  vkPNext :: Ptr ()
  , -- | @imageFormatProperties@ is an instance of a
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'
  -- structure in which capabilities are returned.
  vkImageFormatProperties :: VkImageFormatProperties
  }
  deriving (Eq, Show)

instance Storable VkImageFormatProperties2 where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImageFormatProperties2 <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageFormatProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageFormatProperties2))
                *> poke (ptr `plusPtr` 16) (vkImageFormatProperties (poked :: VkImageFormatProperties2))

instance Zero VkImageFormatProperties2 where
  zero = VkImageFormatProperties2 zero
                                  zero
                                  zero
-- | VkPhysicalDeviceFeatures2 - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Members
--
-- The @VkPhysicalDeviceFeatures2@ structure is defined as:
--
-- = Description
--
-- The @pNext@ chain of this structure is used to extend the structure with
-- features defined by extensions. This structure /can/ be used in
-- 'vkGetPhysicalDeviceFeatures2' or /can/ be in the @pNext@ chain of a
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' structure, in which
-- case it controls which features are enabled in the device in lieu of
-- @pEnabledFeatures@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceFeatures2'
data VkPhysicalDeviceFeatures2 = VkPhysicalDeviceFeatures2
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @features@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'
  -- describing the fine-grained features of the Vulkan 1.0 API.
  vkFeatures :: VkPhysicalDeviceFeatures
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFeatures2 where
  sizeOf ~_ = 240
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFeatures2 <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFeatures2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFeatures2))
                *> poke (ptr `plusPtr` 16) (vkFeatures (poked :: VkPhysicalDeviceFeatures2))

instance Zero VkPhysicalDeviceFeatures2 where
  zero = VkPhysicalDeviceFeatures2 zero
                                   zero
                                   zero
-- | VkPhysicalDeviceImageFormatInfo2 - Structure specifying image creation
-- parameters
--
-- = Description
--
-- The members of @VkPhysicalDeviceImageFormatInfo2@ correspond to the
-- arguments to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- with @sType@ and @pNext@ added for extensibility.
--
-- == Valid Usage
--
-- -   @tiling@ /must/ be @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@ if and
--     only if the @pNext@ chain contains
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- -   If @tiling@ is @VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT@ and @flags@
--     contains @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@, then the @pNext@
--     chain /must/ contain
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--     with non-zero @viewFormatCount@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @type@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
--
-- -   @tiling@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceImageFormatProperties2'
data VkPhysicalDeviceImageFormatInfo2 = VkPhysicalDeviceImageFormatInfo2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure. The
  -- @pNext@ chain of @VkPhysicalDeviceImageFormatInfo2@ is used to provide
  -- additional image parameters to
  -- @vkGetPhysicalDeviceImageFormatProperties2@.
  vkPNext :: Ptr ()
  , -- | @format@ is a 'Graphics.Vulkan.C.Core10.Core.VkFormat' value indicating
  -- the image format, corresponding to
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@.
  vkFormat :: VkFormat
  , -- | @type@ is a 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType'
  -- value indicating the image type, corresponding to
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@imageType@.
  vkType :: VkImageType
  , -- | @tiling@ is a
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
  -- indicating the image tiling, corresponding to
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@tiling@.
  vkTiling :: VkImageTiling
  , -- | @usage@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
  -- indicating the intended usage of the image, corresponding to
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@usage@.
  vkUsage :: VkImageUsageFlags
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'
  -- indicating additional parameters of the image, corresponding to
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@.
  vkFlags :: VkImageCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceImageFormatInfo2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceImageFormatInfo2 <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 28)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 20) (vkType (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 24) (vkTiling (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 28) (vkUsage (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkPhysicalDeviceImageFormatInfo2))

instance Zero VkPhysicalDeviceImageFormatInfo2 where
  zero = VkPhysicalDeviceImageFormatInfo2 zero
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero
-- | VkPhysicalDeviceMemoryProperties2 - Structure specifying physical device
-- memory properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceMemoryProperties2'
data VkPhysicalDeviceMemoryProperties2 = VkPhysicalDeviceMemoryProperties2
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @memoryProperties@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
  -- which is populated with the same values as in
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'.
  vkMemoryProperties :: VkPhysicalDeviceMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryProperties2 where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties2 <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMemoryProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMemoryProperties2))
                *> poke (ptr `plusPtr` 16) (vkMemoryProperties (poked :: VkPhysicalDeviceMemoryProperties2))

instance Zero VkPhysicalDeviceMemoryProperties2 where
  zero = VkPhysicalDeviceMemoryProperties2 zero
                                           zero
                                           zero
-- | VkPhysicalDeviceProperties2 - Structure specifying physical device
-- properties
--
-- = Description
--
-- The @pNext@ chain of this structure is used to extend the structure with
-- properties defined by extensions.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceIDProperties',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties',
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryProperties',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceProperties2'
data VkPhysicalDeviceProperties2 = VkPhysicalDeviceProperties2
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @properties@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
  -- describing the properties of the physical device. This structure is
  -- written with the same values as if it were written by
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceProperties'.
  vkProperties :: VkPhysicalDeviceProperties
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProperties2 where
  sizeOf ~_ = 840
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProperties2 <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProperties2))
                *> poke (ptr `plusPtr` 16) (vkProperties (poked :: VkPhysicalDeviceProperties2))

instance Zero VkPhysicalDeviceProperties2 where
  zero = VkPhysicalDeviceProperties2 zero
                                     zero
                                     zero
-- | VkPhysicalDeviceSparseImageFormatInfo2 - Structure specifying sparse
-- image format inputs
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceSparseImageFormatProperties2'
data VkPhysicalDeviceSparseImageFormatInfo2 = VkPhysicalDeviceSparseImageFormatInfo2
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
  -- value
  vkFormat :: VkFormat
  , -- | @type@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
  vkType :: VkImageType
  , -- | @samples@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
  -- value
  vkSamples :: VkSampleCountFlagBits
  , -- | @usage@ /must/ not be @0@
  vkUsage :: VkImageUsageFlags
  , -- | @tiling@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
  vkTiling :: VkImageTiling
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSparseImageFormatInfo2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSparseImageFormatInfo2 <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 28)
                                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 20) (vkType (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 24) (vkSamples (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 28) (vkUsage (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 32) (vkTiling (poked :: VkPhysicalDeviceSparseImageFormatInfo2))

instance Zero VkPhysicalDeviceSparseImageFormatInfo2 where
  zero = VkPhysicalDeviceSparseImageFormatInfo2 zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
-- | VkQueueFamilyProperties2 - Structure providing information about a queue
-- family
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceQueueFamilyProperties2'
data VkQueueFamilyProperties2 = VkQueueFamilyProperties2
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @queueFamilyProperties@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
  -- which is populated with the same values as in
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'.
  vkQueueFamilyProperties :: VkQueueFamilyProperties
  }
  deriving (Eq, Show)

instance Storable VkQueueFamilyProperties2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkQueueFamilyProperties2 <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueueFamilyProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueueFamilyProperties2))
                *> poke (ptr `plusPtr` 16) (vkQueueFamilyProperties (poked :: VkQueueFamilyProperties2))

instance Zero VkQueueFamilyProperties2 where
  zero = VkQueueFamilyProperties2 zero
                                  zero
                                  zero
-- | VkSparseImageFormatProperties2 - Structure specifying sparse image
-- format properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceSparseImageFormatProperties2'
data VkSparseImageFormatProperties2 = VkSparseImageFormatProperties2
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @properties@ is a structure of type
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
  -- which is populated with the same values as in
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'.
  vkProperties :: VkSparseImageFormatProperties
  }
  deriving (Eq, Show)

instance Storable VkSparseImageFormatProperties2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSparseImageFormatProperties2 <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSparseImageFormatProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSparseImageFormatProperties2))
                *> poke (ptr `plusPtr` 16) (vkProperties (poked :: VkSparseImageFormatProperties2))

instance Zero VkSparseImageFormatProperties2 where
  zero = VkSparseImageFormatProperties2 zero
                                        zero
                                        zero
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceFeatures2 - Reports capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     supported features.
--
-- -   @pFeatures@ is a pointer to a 'VkPhysicalDeviceFeatures2' structure
--     in which the physical device features are returned.
--
-- = Description
--
-- Each structure in @pFeatures@ and its @pNext@ chain contain members
-- corresponding to fine-grained features. @vkGetPhysicalDeviceFeatures2@
-- writes each member to a boolean value indicating whether that feature is
-- supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceFeatures2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFeatures2" vkGetPhysicalDeviceFeatures2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceFeatures2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures2 = FunPtr FN_vkGetPhysicalDeviceFeatures2
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceFormatProperties2 - Lists physical device’s format
-- capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     format properties.
--
-- -   @format@ is the format whose properties are queried.
--
-- -   @pFormatProperties@ is a pointer to a 'VkFormatProperties2'
--     structure in which physical device properties for @format@ are
--     returned.
--
-- = Description
--
-- @vkGetPhysicalDeviceFormatProperties2@ behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat', 'VkFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFormatProperties2" vkGetPhysicalDeviceFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceImageFormatProperties2 - Lists physical device’s
-- image format capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities.
--
-- -   @pImageFormatInfo@ points to an instance of the
--     'VkPhysicalDeviceImageFormatInfo2' structure, describing the
--     parameters that would be consumed by
--     'Graphics.Vulkan.C.Core10.Image.vkCreateImage'.
--
-- -   @pImageFormatProperties@ points to an instance of the
--     'VkImageFormatProperties2' structure in which capabilities are
--     returned.
--
-- = Description
--
-- @vkGetPhysicalDeviceImageFormatProperties2@ behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage
--
-- -   If the @pNext@ chain of @pImageFormatProperties@ contains an
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferUsageANDROID',
--     the @pNext@ chain of @pImageFormatInfo@ /must/ contain an instance
--     of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--     with @handleType@ set to
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pImageFormatInfo@ /must/ be a valid pointer to a valid
--     @VkPhysicalDeviceImageFormatInfo2@ structure
--
-- -   @pImageFormatProperties@ /must/ be a valid pointer to a
--     @VkImageFormatProperties2@ structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_FORMAT_NOT_SUPPORTED@
--
-- = See Also
--
-- 'VkImageFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceImageFormatInfo2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceImageFormatProperties2" vkGetPhysicalDeviceImageFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceImageFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceMemoryProperties2 - Reports memory information for
-- the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the device to query.
--
-- -   @pMemoryProperties@ points to an instance of
--     @VkPhysicalDeviceMemoryProperties2@ structure in which the
--     properties are returned.
--
-- = Description
--
-- @vkGetPhysicalDeviceMemoryProperties2@ behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceMemoryProperties2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMemoryProperties2" vkGetPhysicalDeviceMemoryProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceMemoryProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties2 = FunPtr FN_vkGetPhysicalDeviceMemoryProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceProperties2 - Returns properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pProperties@ points to an instance of the
--     'VkPhysicalDeviceProperties2' structure, that will be filled with
--     returned information.
--
-- = Description
--
-- Each structure in @pProperties@ and its @pNext@ chain contain members
-- corresponding to properties or implementation-dependent limits.
-- @vkGetPhysicalDeviceProperties2@ writes each member to a value
-- indicating the value of that property or limit.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceProperties2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceProperties2" vkGetPhysicalDeviceProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceProperties2 = FunPtr FN_vkGetPhysicalDeviceProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceQueueFamilyProperties2 - Reports properties of the
-- queues of the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pQueueFamilyPropertyCount@ is a pointer to an integer related to
--     the number of queue families available or queried, as described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'.
--
-- -   @pQueueFamilyProperties@ is either @NULL@ or a pointer to an array
--     of 'VkQueueFamilyProperties2' structures.
--
-- = Description
--
-- @vkGetPhysicalDeviceQueueFamilyProperties2@ behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     @VkQueueFamilyProperties2@ structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkQueueFamilyProperties2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceQueueFamilyProperties2" vkGetPhysicalDeviceQueueFamilyProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceQueueFamilyProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties2 = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- | vkGetPhysicalDeviceSparseImageFormatProperties2 - Retrieve properties of
-- an image format applied to sparse images
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     sparse image capabilities.
--
-- -   @pFormatInfo@ is a pointer to a structure of type
--     'VkPhysicalDeviceSparseImageFormatInfo2' containing input parameters
--     to the command.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     sparse format properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkSparseImageFormatProperties2' structures.
--
-- = Description
--
-- @vkGetPhysicalDeviceSparseImageFormatProperties2@ behaves identically to
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties',
-- with the ability to return extended information by adding extension
-- structures to the @pNext@ chain of its @pProperties@ parameter.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pFormatInfo@ /must/ be a valid pointer to a valid
--     @VkPhysicalDeviceSparseImageFormatInfo2@ structure
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ @VkSparseImageFormatProperties2@
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPhysicalDeviceSparseImageFormatInfo2',
-- 'VkSparseImageFormatProperties2'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSparseImageFormatProperties2" vkGetPhysicalDeviceSparseImageFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceSparseImageFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceSparseImageFormatProperties2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 = VkStructureType 1000059002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 = VkStructureType 1000059003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 = VkStructureType 1000059000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 = VkStructureType 1000059004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 = VkStructureType 1000059006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 = VkStructureType 1000059001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 = VkStructureType 1000059008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 = VkStructureType 1000059005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 = VkStructureType 1000059007
