{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  , pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , vkCmdSetSampleLocationsEXT
  , vkGetPhysicalDeviceMultisamplePropertiesEXT
  , VkSampleLocationEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , VkAttachmentSampleLocationsEXT(..)
  , VkSubpassSampleLocationsEXT(..)
  , VkRenderPassSampleLocationsBeginInfoEXT(..)
  , VkPipelineSampleLocationsStateCreateInfoEXT(..)
  , VkPhysicalDeviceSampleLocationsPropertiesEXT(..)
  , VkMultisamplePropertiesEXT(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkSampleCountFlagBits(..)
  , VkPhysicalDevice
  , VkSampleCountFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkDynamicState(..)
  , VkExtent2D(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


-- | @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@ specifies that the
-- @sampleLocationsInfo@ state in
-- 'VkPipelineSampleLocationsStateCreateInfoEXT' will be ignored and /must/
-- be set dynamically with 'vkCmdSetSampleLocationsEXT' before any draw or
-- clear commands. Enabling custom sample locations is still indicated by
-- the @sampleLocationsEnable@ member of
-- @VkPipelineSampleLocationsStateCreateInfoEXT@.
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT = VkDynamicState 1000143000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT = VkStructureType 1000143000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT = VkStructureType 1000143001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT = VkStructureType 1000143002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT = VkStructureType 1000143003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT = VkStructureType 1000143004
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = VkImageCreateFlagBits 0x00001000
-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION"
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME"
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"
-- | vkCmdSetSampleLocationsEXT - Set the dynamic sample locations state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pSampleLocationsInfo@ is the sample locations state to set.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@ dynamic state enabled
--
-- -   The @sampleLocationsPerPixel@ member of @pSampleLocationsInfo@
--     /must/ equal the @rasterizationSamples@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   If
--     'VkPhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
--     is @VK_FALSE@ then the current render pass /must/ have been begun by
--     specifying a 'VkRenderPassSampleLocationsBeginInfoEXT' structure
--     whose @pPostSubpassSampleLocations@ member contains an element with
--     a @subpassIndex@ matching the current subpass index and the
--     @sampleLocationsInfo@ member of that element /must/ match the sample
--     locations state pointed to by @pSampleLocationsInfo@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pSampleLocationsInfo@ /must/ be a valid pointer to a valid
--     @VkSampleLocationsInfoEXT@ structure
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'VkSampleLocationsInfoEXT'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetSampleLocationsEXT" vkCmdSetSampleLocationsEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ()
-- | vkGetPhysicalDeviceMultisamplePropertiesEXT - Report sample count
-- specific multisampling capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     additional multisampling capabilities.
--
-- -   @samples@ is the sample count to query the capabilities for.
--
-- -   @pMultisampleProperties@ is a pointer to a structure of type
--     'VkMultisamplePropertiesEXT', in which information about the
--     additional multisampling capabilities specific to the sample count
--     is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @pMultisampleProperties@ /must/ be a valid pointer to a
--     @VkMultisamplePropertiesEXT@ structure
--
-- = See Also
--
-- 'VkMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMultisamplePropertiesEXT" vkGetPhysicalDeviceMultisamplePropertiesEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ()
-- | VkSampleLocationEXT - Structure specifying the coordinates of a sample
-- location
--
-- = Description
--
-- The domain space of the sample location coordinates has an upper-left
-- origin within the pixel in framebuffer space.
--
-- The values specified in a @VkSampleLocationEXT@ structure are always
-- clamped to the implementation-dependent sample location coordinate range
-- [@sampleLocationCoordinateRange@[0],@sampleLocationCoordinateRange@[1]]
-- that /can/ be queried by chaining the
-- 'VkPhysicalDeviceSampleLocationsPropertiesEXT' structure to the @pNext@
-- chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'.
--
-- = See Also
--
-- 'VkSampleLocationsInfoEXT'
data VkSampleLocationEXT = VkSampleLocationEXT
  { -- | @x@ is the horizontal coordinate of the sample’s location.
  vkX :: CFloat
  , -- | @y@ is the vertical coordinate of the sample’s location.
  vkY :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkSampleLocationEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSampleLocationEXT <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkSampleLocationEXT))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkSampleLocationEXT))
-- | VkSampleLocationsInfoEXT - Structure specifying a set of sample
-- locations
--
-- = Description
--
-- This structure /can/ be used either to specify the sample locations to
-- be used for rendering or to specify the set of sample locations an image
-- subresource has been last rendered with for the purposes of layout
-- transitions of depth\/stencil images created with
-- @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@.
--
-- The sample locations in @pSampleLocations@ specify
-- @sampleLocationsPerPixel@ number of sample locations for each pixel in
-- the grid of the size specified in @sampleLocationGridSize@. The sample
-- location for sample i at the pixel grid location (x,y) is taken from
-- @pSampleLocations@[(x + y * @sampleLocationGridSize.width@) *
-- @sampleLocationsPerPixel@ + i].
--
-- == Valid Usage
--
-- -   @sampleLocationsPerPixel@ /must/ be a bit value that is set in
--     'VkPhysicalDeviceSampleLocationsPropertiesEXT'::@sampleLocationSampleCounts@
--
-- -   @sampleLocationsCount@ /must/ equal @sampleLocationsPerPixel@ ×
--     @sampleLocationGridSize.width@ × @sampleLocationGridSize.height@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT@
--
-- -   @sampleLocationsPerPixel@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @pSampleLocations@ /must/ be a valid pointer to an array of
--     @sampleLocationsCount@ @VkSampleLocationEXT@ structures
--
-- -   @sampleLocationsCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkAttachmentSampleLocationsEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'VkPipelineSampleLocationsStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'VkSampleLocationEXT', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkSubpassSampleLocationsEXT', 'vkCmdSetSampleLocationsEXT'
data VkSampleLocationsInfoEXT = VkSampleLocationsInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @sampleLocationsPerPixel@ is a
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
  -- specifying the number of sample locations per pixel.
  vkSampleLocationsPerPixel :: VkSampleCountFlagBits
  , -- | @sampleLocationGridSize@ is the size of the sample location grid to
  -- select custom sample locations for.
  vkSampleLocationGridSize :: VkExtent2D
  , -- | @sampleLocationsCount@ is the number of sample locations in
  -- @pSampleLocations@.
  vkSampleLocationsCount :: Word32
  , -- | @pSampleLocations@ is an array of @sampleLocationsCount@
  -- 'VkSampleLocationEXT' structures.
  vkPSampleLocations :: Ptr VkSampleLocationEXT
  }
  deriving (Eq, Show)

instance Storable VkSampleLocationsInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSampleLocationsInfoEXT <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsPerPixel (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkSampleLocationGridSize (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationsCount (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPSampleLocations (poked :: VkSampleLocationsInfoEXT))
-- | VkAttachmentSampleLocationsEXT - Structure specifying the sample
-- locations state to use in the initial layout transition of attachments
--
-- = Description
--
-- If the image referenced by the framebuffer attachment at index
-- @attachmentIndex@ was not created with
-- @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ then the
-- values specified in @sampleLocationsInfo@ are ignored.
--
-- == Valid Usage
--
-- -   @attachmentIndex@ /must/ be less than the @attachmentCount@
--     specified in 'Graphics.Vulkan.Core10.Pass.VkRenderPassCreateInfo'
--     the render pass specified by
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'::@renderPass@
--     was created with
--
-- == Valid Usage (Implicit)
--
-- -   @sampleLocationsInfo@ /must/ be a valid @VkSampleLocationsInfoEXT@
--     structure
--
-- = See Also
--
-- 'VkRenderPassSampleLocationsBeginInfoEXT', 'VkSampleLocationsInfoEXT'
data VkAttachmentSampleLocationsEXT = VkAttachmentSampleLocationsEXT
  { -- | @attachmentIndex@ is the index of the attachment for which the sample
  -- locations state is provided.
  vkAttachmentIndex :: Word32
  , -- | @sampleLocationsInfo@ is the sample locations state to use for the
  -- layout transition of the given attachment from the initial layout of the
  -- attachment to the image layout specified for the attachment in the first
  -- subpass using it.
  vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkAttachmentSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAttachmentSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachmentIndex (poked :: VkAttachmentSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkAttachmentSampleLocationsEXT))
-- | VkSubpassSampleLocationsEXT - Structure specifying the sample locations
-- state to use for layout transitions of attachments performed after a
-- given subpass
--
-- = Description
--
-- If the image referenced by the depth\/stencil attachment used in the
-- subpass identified by @subpassIndex@ was not created with
-- @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ or if the
-- subpass does not use a depth\/stencil attachment, and
-- 'VkPhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
-- is @VK_TRUE@ then the values specified in @sampleLocationsInfo@ are
-- ignored.
--
-- == Valid Usage
--
-- -   @subpassIndex@ /must/ be less than the @subpassCount@ specified in
--     'Graphics.Vulkan.Core10.Pass.VkRenderPassCreateInfo' the render pass
--     specified by
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'::@renderPass@
--     was created with
--
-- == Valid Usage (Implicit)
--
-- -   @sampleLocationsInfo@ /must/ be a valid @VkSampleLocationsInfoEXT@
--     structure
--
-- = See Also
--
-- 'VkRenderPassSampleLocationsBeginInfoEXT', 'VkSampleLocationsInfoEXT'
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT
  { -- | @subpassIndex@ is the index of the subpass for which the sample
  -- locations state is provided.
  vkSubpassIndex :: Word32
  , -- | @sampleLocationsInfo@ is the sample locations state to use for the
  -- layout transition of the depth\/stencil attachment away from the image
  -- layout the attachment is used with in the subpass specified in
  -- @subpassIndex@.
  vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkSubpassSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSubpassSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubpassIndex (poked :: VkSubpassSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkSubpassSampleLocationsEXT))
-- | VkRenderPassSampleLocationsBeginInfoEXT - Structure specifying sample
-- locations to use for the layout transition of custom sample locations
-- compatible depth\/stencil attachments
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT@
--
-- -   If @attachmentInitialSampleLocationsCount@ is not @0@,
--     @pAttachmentInitialSampleLocations@ /must/ be a valid pointer to an
--     array of @attachmentInitialSampleLocationsCount@ valid
--     @VkAttachmentSampleLocationsEXT@ structures
--
-- -   If @postSubpassSampleLocationsCount@ is not @0@,
--     @pPostSubpassSampleLocations@ /must/ be a valid pointer to an array
--     of @postSubpassSampleLocationsCount@ valid
--     @VkSubpassSampleLocationsEXT@ structures
--
-- = See Also
--
-- 'VkAttachmentSampleLocationsEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkSubpassSampleLocationsEXT'
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @attachmentInitialSampleLocationsCount@ is the number of elements in the
  -- @pAttachmentInitialSampleLocations@ array.
  vkAttachmentInitialSampleLocationsCount :: Word32
  , -- | @pAttachmentInitialSampleLocations@ is an array of
  -- @attachmentInitialSampleLocationsCount@ 'VkAttachmentSampleLocationsEXT'
  -- structures specifying the attachment indices and their corresponding
  -- sample location state. Each element of
  -- @pAttachmentInitialSampleLocations@ /can/ specify the sample location
  -- state to use in the automatic layout transition performed to transition
  -- a depth\/stencil attachment from the initial layout of the attachment to
  -- the image layout specified for the attachment in the first subpass using
  -- it.
  vkPAttachmentInitialSampleLocations :: Ptr VkAttachmentSampleLocationsEXT
  , -- | @postSubpassSampleLocationsCount@ is the number of elements in the
  -- @pPostSubpassSampleLocations@ array.
  vkPostSubpassSampleLocationsCount :: Word32
  , -- | @pPostSubpassSampleLocations@ is an array of
  -- @postSubpassSampleLocationsCount@ 'VkSubpassSampleLocationsEXT'
  -- structures specifying the subpass indices and their corresponding sample
  -- location state. Each element of @pPostSubpassSampleLocations@ /can/
  -- specify the sample location state to use in the automatic layout
  -- transition performed to transition the depth\/stencil attachment used by
  -- the specified subpass to the image layout specified in a dependent
  -- subpass or to the final layout of the attachment in case the specified
  -- subpass is the last subpass using that attachment. In addition, if
  -- 'VkPhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
  -- is @VK_FALSE@, each element of @pPostSubpassSampleLocations@ /must/
  -- specify the sample location state that matches the sample locations used
  -- by all pipelines that will be bound to a command buffer during the
  -- specified subpass. If @variableSampleLocations@ is @VK_TRUE@, the sample
  -- locations used for rasterization do not depend on
  -- @pPostSubpassSampleLocations@.
  vkPPostSubpassSampleLocations :: Ptr VkSubpassSampleLocationsEXT
  }
  deriving (Eq, Show)

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkRenderPassSampleLocationsBeginInfoEXT <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
                                                     <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkAttachmentInitialSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPAttachmentInitialSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPostSubpassSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkPPostSubpassSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
-- | VkPipelineSampleLocationsStateCreateInfoEXT - Structure specifying
-- sample locations for a pipeline
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT@
--
-- -   @sampleLocationsInfo@ /must/ be a valid @VkSampleLocationsInfoEXT@
--     structure
--
-- = See Also
--
-- @VkBool32@, 'VkSampleLocationsInfoEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @sampleLocationsEnable@ controls whether custom sample locations are
  -- used. If @sampleLocationsEnable@ is @VK_FALSE@, the default sample
  -- locations are used and the values specified in @sampleLocationsInfo@ are
  -- ignored.
  vkSampleLocationsEnable :: VkBool32
  , -- | @sampleLocationsInfo@ is the sample locations to use during
  -- rasterization if @sampleLocationsEnable@ is @VK_TRUE@ and the graphics
  -- pipeline isn’t created with @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@.
  vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPipelineSampleLocationsStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsEnable (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkSampleLocationsInfo (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
-- | VkPhysicalDeviceSampleLocationsPropertiesEXT - Structure describing
-- sample location limits that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceSampleLocationsPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @sampleLocationSampleCounts@ is a bitmask of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     indicating the sample counts supporting custom sample locations.
--
-- -   @maxSampleLocationGridSize@ is the maximum size of the pixel grid in
--     which sample locations /can/ vary that is supported for all sample
--     counts in @sampleLocationSampleCounts@.
--
-- -   @sampleLocationCoordinateRange@[2] is the range of supported sample
--     location coordinates.
--
-- -   @sampleLocationSubPixelBits@ is the number of bits of subpixel
--     precision for sample locations.
--
-- -   @variableSampleLocations@ specifies whether the sample locations
--     used by all pipelines that will be bound to a command buffer during
--     a subpass /must/ match. If set to @VK_TRUE@, the implementation
--     supports variable sample locations in a subpass. If set to
--     @VK_FALSE@, then the sample locations /must/ stay constant in each
--     subpass.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT@
--
-- If the @VkPhysicalDeviceSampleLocationsPropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceSampleLocationsPropertiesEXT = VkPhysicalDeviceSampleLocationsPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSampleCounts"
  vkSampleLocationSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "maxSampleLocationGridSize"
  vkMaxSampleLocationGridSize :: VkExtent2D
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationCoordinateRange"
  vkSampleLocationCoordinateRange :: Vector 2 CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSubPixelBits"
  vkSampleLocationSubPixelBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "variableSampleLocations"
  vkVariableSampleLocations :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSampleLocationsPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSampleLocationsPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
                                                          <*> peek (ptr `plusPtr` 28)
                                                          <*> peek (ptr `plusPtr` 36)
                                                          <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationSampleCounts (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxSampleLocationGridSize (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationCoordinateRange (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkSampleLocationSubPixelBits (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkVariableSampleLocations (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
-- | VkMultisamplePropertiesEXT - Structure returning information about
-- sample count specific additional multisampling capabilities
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetPhysicalDeviceMultisamplePropertiesEXT'
data VkMultisamplePropertiesEXT = VkMultisamplePropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxSampleLocationGridSize@ is the maximum size of the pixel grid in
  -- which sample locations /can/ vary.
  vkMaxSampleLocationGridSize :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkMultisamplePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMultisamplePropertiesEXT <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxSampleLocationGridSize (poked :: VkMultisamplePropertiesEXT))
