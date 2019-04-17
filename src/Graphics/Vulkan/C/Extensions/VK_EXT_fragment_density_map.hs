{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  , pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION
  , pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkImageUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageViewCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkAttachmentReference(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSamplerCreateFlagBits(..)
  )


-- | VkPhysicalDeviceFragmentDensityMapFeaturesEXT - Structure describing
-- fragment density map features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceFragmentDensityMapFeaturesEXT@
-- structure describe the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceFragmentDensityMapFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceFragmentDensityMapFeaturesEXT@ /can/ also be used in
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable the features.
--
-- Unresolved directive in
-- VkPhysicalDeviceFragmentDensityMapFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceFragmentDensityMapFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceFragmentDensityMapFeaturesEXT = VkPhysicalDeviceFragmentDensityMapFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @fragmentDensityMap@ specifies whether the implementation supports
  -- render passes with a fragment density map attachment. If this feature is
  -- not enabled and the @pNext@ chain of @VkRenderPassCreateInfo@ contains
  -- @VkRenderPassFragmentDensityMapCreateInfoEXT@,
  -- @fragmentDensityMapAttachment@ /must/ be @VK_ATTACHMENT_UNUSED@.
  vkFragmentDensityMap :: VkBool32
  , -- | @fragmentDensityMapDynamic@ specifies whether the implementation
  -- supports dynamic fragment density map image views. If this feature is
  -- not enabled, @VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT@
  -- /must/ not be included in @VkImageViewCreateInfo@::@flags@.
  vkFragmentDensityMapDynamic :: VkBool32
  , -- | @fragmentDensityMapNonSubsampledImages@ specifies whether the
  -- implementation supports regular non-subsampled image attachments with
  -- fragment density map render passes. If this feature is not enabled,
  -- render passes with a
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>
  -- /must/ only have
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-subsampledimages subsampled attachments>
  -- bound.
  vkFragmentDensityMapNonSubsampledImages :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFragmentDensityMapFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkFragmentDensityMap (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkFragmentDensityMapDynamic (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkFragmentDensityMapNonSubsampledImages (poked :: VkPhysicalDeviceFragmentDensityMapFeaturesEXT))

instance Zero VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  zero = VkPhysicalDeviceFragmentDensityMapFeaturesEXT zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
-- | VkPhysicalDeviceFragmentDensityMapPropertiesEXT - Structure describing
-- fragment density map properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceFragmentDensityMapPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- Unresolved directive in
-- VkPhysicalDeviceFragmentDensityMapPropertiesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceFragmentDensityMapPropertiesEXT.txt[]
--
-- If the @VkPhysicalDeviceFragmentDensityMapPropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2KHR',
-- it is filled with the implementation-dependent limits and properties.
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceFragmentDensityMapPropertiesEXT = VkPhysicalDeviceFragmentDensityMapPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @minFragmentDensityTexelSize@ is the minimum
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#glossary-fragment-density-texel-size fragment density texel size>.
  vkMinFragmentDensityTexelSize :: VkExtent2D
  , -- | @maxFragmentDensityTexelSize@ is the maximum fragment density texel
  -- size.
  vkMaxFragmentDensityTexelSize :: VkExtent2D
  , -- | @fragmentDensityInvocations@ specifies whether the implementation /may/
  -- invoke additional fragment shader invocations for each covered sample.
  vkFragmentDensityInvocations :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFragmentDensityMapPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 24)
                                                             <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMinFragmentDensityTexelSize (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkMaxFragmentDensityTexelSize (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkFragmentDensityInvocations (poked :: VkPhysicalDeviceFragmentDensityMapPropertiesEXT))

instance Zero VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  zero = VkPhysicalDeviceFragmentDensityMapPropertiesEXT zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
-- | VkRenderPassFragmentDensityMapCreateInfoEXT - Structure containing
-- fragment density map attachment for render pass
--
-- = Description
--
-- The fragment density map attachment is read at an
-- implementation-dependent time either by the host during
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass' if
-- the attachment’s image view was not created with @flags@ containing
-- @VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT@, or by the
-- device when drawing commands in the renderpass execute
-- @VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT@.
--
-- If this structure is not present, it is as if
-- @fragmentDensityMapAttachment@ was given as @VK_ATTACHMENT_UNUSED@.
--
-- == Valid Usage
--
-- -   If @fragmentDensityMapAttachment@ is not @VK_ATTACHMENT_UNUSED@,
--     @fragmentDensityMapAttachment@ /must/ be less than
--     @VkRenderPassCreateInfo@::@attachmentCount@
--
-- -   If @fragmentDensityMapAttachment@ is not @VK_ATTACHMENT_UNUSED@,
--     @fragmentDensityMapAttachment@ /must/ not be an element of
--     @VkSubpassDescription@::@pInputAttachments@,
--     @VkSubpassDescription@::@pColorAttachments@,
--     @VkSubpassDescription@::@pResolveAttachments@,
--     @VkSubpassDescription@::@pDepthStencilAttachment@, or
--     @VkSubpassDescription@::@pPreserveAttachments@ for any subpass
--
-- -   If @fragmentDensityMapAttachment@ is not @VK_ATTACHMENT_UNUSED@,
--     @layout@ /must/ be equal to
--     @VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT@, or
--     @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   If @fragmentDensityMapAttachment@ is not @VK_ATTACHMENT_UNUSED@,
--     @fragmentDensityMapAttachment@ /must/ reference an attachment with a
--     @loadOp@ equal to @VK_ATTACHMENT_LOAD_OP_LOAD@ or
--     @VK_ATTACHMENT_LOAD_OP_DONT_CARE@.
--
-- -   If @fragmentDensityMapAttachment@ is not @VK_ATTACHMENT_UNUSED@,
--     @fragmentDensityMapAttachment@ /must/ reference an attachment with a
--     @storeOp@ equal to @VK_ATTACHMENT_STORE_OP_DONT_CARE@.
--
-- Unresolved directive in VkRenderPassFragmentDensityMapCreateInfoEXT.txt
-- -
-- include::..\/validity\/structs\/VkRenderPassFragmentDensityMapCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkRenderPassFragmentDensityMapCreateInfoEXT = VkRenderPassFragmentDensityMapCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @fragmentDensityMapAttachment@ is the fragment density map to use for
  -- the render pass.
  vkFragmentDensityMapAttachment :: VkAttachmentReference
  }
  deriving (Eq, Show)

instance Storable VkRenderPassFragmentDensityMapCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkRenderPassFragmentDensityMapCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassFragmentDensityMapCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassFragmentDensityMapCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFragmentDensityMapAttachment (poked :: VkRenderPassFragmentDensityMapCreateInfoEXT))

instance Zero VkRenderPassFragmentDensityMapCreateInfoEXT where
  zero = VkRenderPassFragmentDensityMapCreateInfoEXT zero
                                                     zero
                                                     zero
-- | @VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT@ specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>
-- during dynamic
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragmentdensitymapops fragment density map operations>
pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT = VkAccessFlagBits 0x01000000
-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME"
pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = "VK_EXT_fragment_density_map"
-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION"
pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION :: Integral a => a
pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT = VkFormatFeatureFlagBits 0x01000000
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT = VkImageCreateFlagBits 0x00004000
-- | @VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT@ /must/ only be used
-- as a fragment density map attachment in a @VkRenderPass@. This layout is
-- valid only for image subresources of images created with the
-- @VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT@ usage bit enabled.
pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT :: VkImageLayout
pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = VkImageLayout 1000218000
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = VkImageUsageFlagBits 0x00000200
-- | @VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT@ prohibits
-- the implementation from accessing the fragment density map by the host
-- during @vkCmdBeginRenderPass@ as the contents are expected to change
-- after recording
pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT :: VkImageViewCreateFlagBits
pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT = VkImageViewCreateFlagBits 0x00000001
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT = VkPipelineStageFlagBits 0x00800000
-- | @VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT@ specifies that the sampler will
-- read from an image created with @flags@ containing
-- @VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT@.
pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT :: VkSamplerCreateFlagBits
pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT = VkSamplerCreateFlagBits 0x00000001
-- | @VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT@ specifies
-- that the implementation /may/ use approximations when reconstructing a
-- full color value for texture access from a subsampled image.
pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT :: VkSamplerCreateFlagBits
pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = VkSamplerCreateFlagBits 0x00000002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT = VkStructureType 1000218000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT = VkStructureType 1000218001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT = VkStructureType 1000218002
