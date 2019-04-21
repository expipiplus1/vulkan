{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentDescription2KHR(..)
  , VkAttachmentReference2KHR(..)
  , VkRenderPassCreateInfo2KHR(..)
  , VkSubpassBeginInfoKHR(..)
  , VkSubpassDependency2KHR(..)
  , VkSubpassDescription2KHR(..)
  , VkSubpassEndInfoKHR(..)
  , FN_vkCmdBeginRenderPass2KHR
  , PFN_vkCmdBeginRenderPass2KHR
  , vkCmdBeginRenderPass2KHR
  , FN_vkCmdEndRenderPass2KHR
  , PFN_vkCmdEndRenderPass2KHR
  , vkCmdEndRenderPass2KHR
  , FN_vkCmdNextSubpass2KHR
  , PFN_vkCmdNextSubpass2KHR
  , vkCmdNextSubpass2KHR
  , FN_vkCreateRenderPass2KHR
  , PFN_vkCreateRenderPass2KHR
  , vkCreateRenderPass2KHR
  , pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  , pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
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


import Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkRenderPassBeginInfo(..)
  , VkSubpassContents(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAttachmentLoadOp(..)
  , VkAttachmentStoreOp(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateFlags(..)
  , VkAccessFlags
  , VkAttachmentDescriptionFlags
  , VkDependencyFlags
  , VkSubpassDescriptionFlags
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkRenderPass
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkPipelineStageFlags
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkAttachmentDescription2KHR - Structure specifying an attachment
-- description
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' have the
-- identical effect to those parameters.
--
-- == Valid Usage
--
-- Unresolved directive in VkAttachmentDescription2KHR.txt -
-- include::{generated}\/validity\/structs\/VkAttachmentDescription2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkAttachmentDescription2KHR = VkAttachmentDescription2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescriptionFlagBits'
  -- specifying additional properties of the attachment.
  vkFlags :: VkAttachmentDescriptionFlags
  , -- | @format@ is a 'Graphics.Vulkan.C.Core10.Core.VkFormat' value specifying
  -- the format of the image that will be used for the attachment.
  vkFormat :: VkFormat
  , -- | @samples@ is the number of samples of the image as defined in
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'.
  vkSamples :: VkSampleCountFlagBits
  , -- | @loadOp@ is a 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentLoadOp' value
  -- specifying how the contents of color and depth components of the
  -- attachment are treated at the beginning of the subpass where it is first
  -- used.
  vkLoadOp :: VkAttachmentLoadOp
  , -- | @storeOp@ is a 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentStoreOp' value
  -- specifying how the contents of color and depth components of the
  -- attachment are treated at the end of the subpass where it is last used.
  vkStoreOp :: VkAttachmentStoreOp
  , -- | @stencilLoadOp@ is a 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentLoadOp'
  -- value specifying how the contents of stencil components of the
  -- attachment are treated at the beginning of the subpass where it is first
  -- used.
  vkStencilLoadOp :: VkAttachmentLoadOp
  , -- | @stencilStoreOp@ is a
  -- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentStoreOp' value specifying how
  -- the contents of stencil components of the attachment are treated at the
  -- end of the last subpass where it is used.
  vkStencilStoreOp :: VkAttachmentStoreOp
  , -- | @initialLayout@ is the layout the attachment image subresource will be
  -- in when a render pass instance begins.
  vkInitialLayout :: VkImageLayout
  , -- | @finalLayout@ /must/ not be
  -- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
  -- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED'
  vkFinalLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkAttachmentDescription2KHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkAttachmentDescription2KHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 20)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 28)
                                         <*> peek (ptr `plusPtr` 32)
                                         <*> peek (ptr `plusPtr` 36)
                                         <*> peek (ptr `plusPtr` 40)
                                         <*> peek (ptr `plusPtr` 44)
                                         <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 20) (vkFormat (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 24) (vkSamples (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 28) (vkLoadOp (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 32) (vkStoreOp (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 36) (vkStencilLoadOp (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 40) (vkStencilStoreOp (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 44) (vkInitialLayout (poked :: VkAttachmentDescription2KHR))
                *> poke (ptr `plusPtr` 48) (vkFinalLayout (poked :: VkAttachmentDescription2KHR))

instance Zero VkAttachmentDescription2KHR where
  zero = VkAttachmentDescription2KHR VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero

-- | VkAttachmentReference2KHR - Structure specifying an attachment reference
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' have the identical
-- effect to those parameters.
--
-- @aspectMask@ has the same effect for the described attachment as
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkInputAttachmentAspectReference'::@aspectMask@
-- has on each corresponding attachment. It is ignored when this structure
-- is used to describe anything other than an input attachment reference.
--
-- == Valid Usage
--
-- -   If @attachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', @layout@
--     /must/ not be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED'
--
-- Unresolved directive in VkAttachmentReference2KHR.txt -
-- include::{generated}\/validity\/structs\/VkAttachmentReference2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkAttachmentReference2KHR = VkAttachmentReference2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @attachment@ is either an integer value identifying an attachment at the
  -- corresponding index in
  -- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'::@pAttachments@,
  -- or 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' to signify
  -- that this attachment is not used.
  vkAttachment :: Word32
  , -- | @layout@ is a 'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
  -- specifying the layout the attachment uses during the subpass.
  vkLayout :: VkImageLayout
  , -- | @aspectMask@ is a mask of which aspect(s) /can/ be accessed within the
  -- specified subpass as an input attachment.
  vkAspectMask :: VkImageAspectFlags
  }
  deriving (Eq, Show)

instance Storable VkAttachmentReference2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkAttachmentReference2KHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAttachmentReference2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAttachmentReference2KHR))
                *> poke (ptr `plusPtr` 16) (vkAttachment (poked :: VkAttachmentReference2KHR))
                *> poke (ptr `plusPtr` 20) (vkLayout (poked :: VkAttachmentReference2KHR))
                *> poke (ptr `plusPtr` 24) (vkAspectMask (poked :: VkAttachmentReference2KHR))

instance Zero VkAttachmentReference2KHR where
  zero = VkAttachmentReference2KHR VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
                                   zero
                                   zero
                                   zero
                                   zero

-- | VkRenderPassCreateInfo2KHR - Structure specifying parameters of a newly
-- created render pass
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo' have the
-- identical effect to those parameters; the child structures are variants
-- of those used in 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'
-- which include @sType@ and @pNext@ parameters, allowing them to be
-- extended.
--
-- If the 'VkSubpassDescription2KHR'::@viewMask@ member of any element of
-- @pSubpasses@ is not zero, /multiview/ functionality is considered to be
-- enabled for this render pass.
--
-- @correlatedViewMaskCount@ and @pCorrelatedViewMasks@ have the same
-- effect as
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo'::@correlationMaskCount@
-- and
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo'::@pCorrelationMasks@,
-- respectively.
--
-- == Valid Usage
--
-- -   If any two subpasses operate on attachments with overlapping ranges
--     of the same 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object,
--     and at least one subpass writes to that area of
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory', a subpass
--     dependency /must/ be included (either directly or via some
--     intermediate subpasses) between them
--
-- -   If the @attachment@ member of any element of @pInputAttachments@,
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@, or the attachment indexed by any element
--     of @pPreserveAttachments@ in any given element of @pSubpasses@ is
--     bound to a range of a
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object that
--     overlaps with any other attachment in any subpass (including the
--     same subpass), the 'VkAttachmentDescription2KHR' structures
--     describing them /must/ include
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     in @flags@
--
-- -   If the @attachment@ member of any element of @pInputAttachments@,
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@, or any element of @pPreserveAttachments@
--     in any given element of @pSubpasses@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', it /must/
--     be less than @attachmentCount@
--
-- -   For any member of @pAttachments@ with a @loadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--
-- -   For any member of @pAttachments@ with a @stencilLoadOp@ equal to
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', the
--     first use of that attachment /must/ not specify a @layout@ equal to
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'.
--
-- -   For any element of @pDependencies@, if the @srcSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', all stage
--     flags included in the @srcStageMask@ member of that dependency
--     /must/ be a pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass.
--
-- -   For any element of @pDependencies@, if the @dstSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', all stage
--     flags included in the @dstStageMask@ member of that dependency
--     /must/ be a pipeline stage supported by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass.
--
-- -   The set of bits included in any element of @pCorrelatedViewMasks@
--     /must/ not overlap with the set of bits included in any other
--     element of @pCorrelatedViewMasks@
--
-- -   If the 'VkSubpassDescription2KHR'::@viewMask@ member of all elements
--     of @pSubpasses@ is @0@, @correlatedViewMaskCount@ /must/ be @0@
--
-- -   The 'VkSubpassDescription2KHR'::@viewMask@ member of all elements of
--     @pSubpasses@ /must/ either all be @0@, or all not be @0@
--
-- -   If the 'VkSubpassDescription2KHR'::@viewMask@ member of all elements
--     of @pSubpasses@ is @0@, the @dependencyFlags@ member of any element
--     of @pDependencies@ /must/ not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   For any element of @pDependencies@ where its @srcSubpass@ member
--     equals its @dstSubpass@ member, if the @viewMask@ member of the
--     corresponding element of @pSubpasses@ includes more than one bit,
--     its @dependencyFlags@ member /must/ include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   The @viewMask@ member /must/ not include a bit at a position greater
--     than the value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxFramebufferLayers@
--
-- -   If the @attachment@ member of any element of the @pInputAttachments@
--     member of any element of @pSubpasses@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
--     @aspectMask@ member of that element of @pInputAttachments@ /must/
--     only include aspects that are present in images of the format
--     specified by the element of @pAttachments@ specified by @attachment@
--
-- -   The @srcSubpass@ member of each element of @pDependencies@ /must/ be
--     less than @subpassCount@
--
-- -   The @dstSubpass@ member of each element of @pDependencies@ /must/ be
--     less than @subpassCount@
--
-- Unresolved directive in VkRenderPassCreateInfo2KHR.txt -
-- include::{generated}\/validity\/structs\/VkRenderPassCreateInfo2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkRenderPassCreateInfo2KHR = VkRenderPassCreateInfo2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkRenderPassCreateFlags
  , -- | @attachmentCount@ is the number of attachments used by this render pass.
  vkAttachmentCount :: Word32
  , -- | @pAttachments@ points to an array of @attachmentCount@
  -- 'VkAttachmentDescription2KHR' structures describing the attachments used
  -- by the render pass.
  vkPAttachments :: Ptr VkAttachmentDescription2KHR
  , -- | @subpassCount@ is the number of subpasses to create.
  vkSubpassCount :: Word32
  , -- | @pSubpasses@ points to an array of @subpassCount@
  -- 'VkSubpassDescription2KHR' structures describing each subpass.
  vkPSubpasses :: Ptr VkSubpassDescription2KHR
  , -- | @dependencyCount@ is the number of dependencies between pairs of
  -- subpasses.
  vkDependencyCount :: Word32
  , -- | @pDependencies@ points to an array of @dependencyCount@
  -- 'VkSubpassDependency2KHR' structures describing dependencies between
  -- pairs of subpasses.
  vkPDependencies :: Ptr VkSubpassDependency2KHR
  , -- | @correlatedViewMaskCount@ is the number of correlation masks.
  vkCorrelatedViewMaskCount :: Word32
  , -- | @pCorrelatedViewMasks@ is an array of view masks indicating sets of
  -- views that /may/ be more efficient to render concurrently.
  vkPCorrelatedViewMasks :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkRenderPassCreateInfo2KHR where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek ptr = VkRenderPassCreateInfo2KHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
                                        <*> peek (ptr `plusPtr` 48)
                                        <*> peek (ptr `plusPtr` 56)
                                        <*> peek (ptr `plusPtr` 64)
                                        <*> peek (ptr `plusPtr` 72)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 20) (vkAttachmentCount (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 24) (vkPAttachments (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 32) (vkSubpassCount (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 40) (vkPSubpasses (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 48) (vkDependencyCount (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 56) (vkPDependencies (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 64) (vkCorrelatedViewMaskCount (poked :: VkRenderPassCreateInfo2KHR))
                *> poke (ptr `plusPtr` 72) (vkPCorrelatedViewMasks (poked :: VkRenderPassCreateInfo2KHR))

instance Zero VkRenderPassCreateInfo2KHR where
  zero = VkRenderPassCreateInfo2KHR VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- | VkSubpassBeginInfoKHR - Structure specifying subpass begin info
--
-- = Description
--
-- Unresolved directive in VkSubpassBeginInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkSubpassBeginInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSubpassBeginInfoKHR = VkSubpassBeginInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @contents@ is a
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkSubpassContents' value
  -- specifying how the commands in the next subpass will be provided.
  vkContents :: VkSubpassContents
  }
  deriving (Eq, Show)

instance Storable VkSubpassBeginInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSubpassBeginInfoKHR <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubpassBeginInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubpassBeginInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkContents (poked :: VkSubpassBeginInfoKHR))

instance Zero VkSubpassBeginInfoKHR where
  zero = VkSubpassBeginInfoKHR VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
                               zero
                               zero

-- | VkSubpassDependency2KHR - Structure specifying a subpass dependency
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency' have the identical
-- effect to those parameters.
--
-- @viewOffset@ has the same effect for the described subpass dependency as
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo'::@pViewOffsets@
-- has on each corresponding subpass dependency.
--
-- == Valid Usage
--
-- -   If @srcSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL',
--     @srcStageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'
--
-- -   If @dstSubpass@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL',
--     @dstStageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   @srcSubpass@ /must/ be less than or equal to @dstSubpass@, unless
--     one of them is
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL', to avoid
--     cyclic dependencies and ensure a valid execution order
--
-- -   @srcSubpass@ and @dstSubpass@ /must/ not both be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'
--
-- -   If @srcSubpass@ is equal to @dstSubpass@, @srcStageMask@ and
--     @dstStageMask@ /must/ not set any bits that are neither
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT',
--     nor one of the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-types graphics pipeline stages>
--
-- -   If @srcSubpass@ is equal to @dstSubpass@ and not all of the stages
--     in @srcStageMask@ and @dstStageMask@ are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>,
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
--     pipeline stage in @srcStageMask@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
--     than or equal to the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earliest>
--     pipeline stage in @dstStageMask@
--
-- -   Any access flag included in @srcAccessMask@ /must/ be supported by
--     one of the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   Any access flag included in @dstAccessMask@ /must/ be supported by
--     one of the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   If @dependencyFlags@ includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT',
--     @srcSubpass@ /must/ not be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'
--
-- -   If @dependencyFlags@ includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT',
--     @dstSubpass@ /must/ not be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'
--
-- -   If @srcSubpass@ equals @dstSubpass@, and @srcStageMask@ and
--     @dstStageMask@ both include a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stage>,
--     then @dependencyFlags@ /must/ include
--     'Graphics.Vulkan.C.Core10.Pass.VK_DEPENDENCY_BY_REGION_BIT'
--
-- -   If @viewOffset@ is not equal to @0@, @srcSubpass@ /must/ not be
--     equal to @dstSubpass@
--
-- -   If @dependencyFlags@ does not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VK_DEPENDENCY_VIEW_LOCAL_BIT',
--     @viewOffset@ /must/ be @0@
--
-- -   If @viewOffset@ is not @0@, @srcSubpass@ /must/ not be equal to
--     @dstSubpass@.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- Unresolved directive in VkSubpassDependency2KHR.txt -
-- include::{generated}\/validity\/structs\/VkSubpassDependency2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSubpassDependency2KHR = VkSubpassDependency2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcSubpass@ is the subpass index of the first subpass in the
  -- dependency, or 'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'.
  vkSrcSubpass :: Word32
  , -- | @dstSubpass@ is the subpass index of the second subpass in the
  -- dependency, or 'Graphics.Vulkan.C.Core10.Constants.VK_SUBPASS_EXTERNAL'.
  vkDstSubpass :: Word32
  , -- | @srcStageMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
  vkSrcStageMask :: VkPipelineStageFlags
  , -- | @dstStageMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
  vkDstStageMask :: VkPipelineStageFlags
  , -- | @srcAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits' specifying a
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
  vkSrcAccessMask :: VkAccessFlags
  , -- | @dstAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits' specifying a
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
  vkDstAccessMask :: VkAccessFlags
  , -- | @dependencyFlags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlagBits'.
  vkDependencyFlags :: VkDependencyFlags
  , -- | @viewOffset@ controls which views in the source subpass the views in the
  -- destination subpass depend on.
  vkViewOffset :: Int32
  }
  deriving (Eq, Show)

instance Storable VkSubpassDependency2KHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSubpassDependency2KHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 28)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 36)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 16) (vkSrcSubpass (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 20) (vkDstSubpass (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 24) (vkSrcStageMask (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 28) (vkDstStageMask (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 32) (vkSrcAccessMask (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 36) (vkDstAccessMask (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 40) (vkDependencyFlags (poked :: VkSubpassDependency2KHR))
                *> poke (ptr `plusPtr` 44) (vkViewOffset (poked :: VkSubpassDependency2KHR))

instance Zero VkSubpassDependency2KHR where
  zero = VkSubpassDependency2KHR VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- | VkSubpassDescription2KHR - Structure specifying a subpass description
--
-- = Description
--
-- Parameters defined by this structure with the same name as those in
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription' have the identical
-- effect to those parameters.
--
-- @viewMask@ has the same effect for the described subpass as
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo'::@pViewMasks@
-- has on each corresponding subpass.
--
-- == Valid Usage
--
-- -   @pipelineBindPoint@ /must/ be
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   @colorAttachmentCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxColorAttachments@
--
-- -   If the first use of an attachment in this render pass is as an input
--     attachment, and the attachment is not also used as a color or
--     depth\/stencil attachment in the same subpass, then @loadOp@ /must/
--     not be 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   If @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that does not have the value
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not have the value
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   If @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', the
--     corresponding color attachment /must/ not have a sample count of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @pResolveAttachments@ is not @NULL@, each resolve attachment that
--     is not 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--     /must/ have a sample count of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   Any given element of @pResolveAttachments@ /must/ have the same
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' as its corresponding color
--     attachment
--
-- -   All attachments in @pColorAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have the same sample count
--
-- -   If the @VK_AMD_mixed_attachment_samples@ extension is enabled, all
--     attachments in @pColorAttachments@ that are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' /must/
--     have a sample count that is smaller than or equal to the sample
--     count of @pDepthStencilAttachment@ if it is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   If neither the @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, and if
--     @pDepthStencilAttachment@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' and any
--     attachments in @pColorAttachments@ are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', they
--     /must/ have the same sample count
--
-- -   The @attachment@ member of any element of @pPreserveAttachments@
--     /must/ not be
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED'
--
-- -   Any given element of @pPreserveAttachments@ /must/ not also be an
--     element of any other member of the subpass description
--
-- -   If any attachment is used by more than one
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' member, then
--     each use /must/ use the same @layout@
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX',
--     it /must/ also include
--     'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'.
--
-- -   The @aspectMask@ member of any element of @pInputAttachments@ /must/
--     be a valid combination of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--
-- -   The @aspectMask@ member of any element of @pInputAttachments@ /must/
--     not be @0@
--
-- -   The @aspectMask@ member of each element of @pInputAttachments@
--     /must/ not include
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_METADATA_BIT'
--
-- Unresolved directive in VkSubpassDescription2KHR.txt -
-- include::{generated}\/validity\/structs\/VkSubpassDescription2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSubpassDescription2KHR = VkSubpassDescription2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescriptionFlagBits' specifying
  -- usage of the subpass.
  vkFlags :: VkSubpassDescriptionFlags
  , -- | @pipelineBindPoint@ is a
  -- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' value specifying the
  -- pipeline type supported for this subpass.
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- | @viewMask@ is a bitfield of view indices describing which views
  -- rendering is broadcast to in this subpass, when multiview is enabled.
  vkViewMask :: Word32
  , -- | @inputAttachmentCount@ is the number of input attachments.
  vkInputAttachmentCount :: Word32
  , -- | @pInputAttachments@ is an array of 'VkAttachmentReference2KHR'
  -- structures defining the input attachments for this subpass and their
  -- layouts.
  vkPInputAttachments :: Ptr VkAttachmentReference2KHR
  , -- | @colorAttachmentCount@ is the number of color attachments.
  vkColorAttachmentCount :: Word32
  , -- | @pColorAttachments@ is an array of 'VkAttachmentReference2KHR'
  -- structures defining the color attachments for this subpass and their
  -- layouts.
  vkPColorAttachments :: Ptr VkAttachmentReference2KHR
  , -- | @pResolveAttachments@ is an optional array of @colorAttachmentCount@
  -- 'VkAttachmentReference2KHR' structures defining the resolve attachments
  -- for this subpass and their layouts.
  vkPResolveAttachments :: Ptr VkAttachmentReference2KHR
  , -- | @pDepthStencilAttachment@ is a pointer to a 'VkAttachmentReference2KHR'
  -- specifying the depth\/stencil attachment for this subpass and its
  -- layout.
  vkPDepthStencilAttachment :: Ptr VkAttachmentReference2KHR
  , -- | @preserveAttachmentCount@ is the number of preserved attachments.
  vkPreserveAttachmentCount :: Word32
  , -- | @pPreserveAttachments@ is an array of @preserveAttachmentCount@ render
  -- pass attachment indices identifying attachments that are not used by
  -- this subpass, but whose contents /must/ be preserved throughout the
  -- subpass.
  vkPPreserveAttachments :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkSubpassDescription2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkSubpassDescription2KHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 48)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 20) (vkPipelineBindPoint (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 24) (vkViewMask (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 28) (vkInputAttachmentCount (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 32) (vkPInputAttachments (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 40) (vkColorAttachmentCount (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 48) (vkPColorAttachments (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 56) (vkPResolveAttachments (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 64) (vkPDepthStencilAttachment (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 72) (vkPreserveAttachmentCount (poked :: VkSubpassDescription2KHR))
                *> poke (ptr `plusPtr` 80) (vkPPreserveAttachments (poked :: VkSubpassDescription2KHR))

instance Zero VkSubpassDescription2KHR where
  zero = VkSubpassDescription2KHR VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero

-- | VkSubpassEndInfoKHR - Structure specifying subpass end info
--
-- = Description
--
-- Unresolved directive in VkSubpassEndInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkSubpassEndInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSubpassEndInfoKHR = VkSubpassEndInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkSubpassEndInfoKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkSubpassEndInfoKHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubpassEndInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubpassEndInfoKHR))

instance Zero VkSubpassEndInfoKHR where
  zero = VkSubpassEndInfoKHR VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
                             zero

-- | vkCmdBeginRenderPass2KHR - Begin a new render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @pRenderPassBegin@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'
--     structure (defined below) which indicates the render pass to begin
--     an instance of, and the framebuffer the instance uses.
--
-- -   @pSubpassBeginInfo@ is a pointer to a 'VkSubpassBeginInfoKHR'
--     structure which contains information about the subpass which is
--     about to begin rendering.
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record the commands for the first subpass of that render pass.
--
-- == Valid Usage
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   If any of the @initialLayout@ members of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is not
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED', then
--     each such @initialLayout@ /must/ be equal to the current layout of
--     the corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   The @srcStageMask@ and @dstStageMask@ members of any element of the
--     @pDependencies@ member of
--     'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo' used to
--     create @renderPass@ /must/ be supported by the capabilities of the
--     queue family identified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo' used
--     to create the command pool which @commandBuffer@ was allocated from
--
-- -   For any attachment in @framebuffer@ that is used by @renderPass@ and
--     is bound to memory locations that are also bound to another
--     attachment used by @renderPass@, and if at least one of those uses
--     causes either attachment to be written to, both attachments /must/
--     have had the
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     set
--
-- Unresolved directive in vkCmdBeginRenderPass2KHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginRenderPass2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginRenderPass2KHR" vkCmdBeginRenderPass2KHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()
#else
vkCmdBeginRenderPass2KHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()
vkCmdBeginRenderPass2KHR deviceCmds = mkVkCmdBeginRenderPass2KHR (pVkCmdBeginRenderPass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass2KHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ())
#endif

type FN_vkCmdBeginRenderPass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()
type PFN_vkCmdBeginRenderPass2KHR = FunPtr FN_vkCmdBeginRenderPass2KHR

-- | vkCmdEndRenderPass2KHR - End the current render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to end the current
--     render pass instance.
--
-- -   @pSubpassEndInfo@ is a pointer to a 'VkSubpassEndInfoKHR' structure
--     which contains information about how the previous subpass will be
--     ended.
--
-- = Description
--
-- 'vkCmdEndRenderPass2KHR' is semantically identical to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndRenderPass',
-- except that it is extensible.
--
-- == Valid Usage
--
-- -   The current subpass index /must/ be equal to the number of subpasses
--     in the render pass minus one
--
-- -   This command /must/ not be recorded when transform feedback is
--     active
--
-- Unresolved directive in vkCmdEndRenderPass2KHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndRenderPass2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndRenderPass2KHR" vkCmdEndRenderPass2KHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
#else
vkCmdEndRenderPass2KHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
vkCmdEndRenderPass2KHR deviceCmds = mkVkCmdEndRenderPass2KHR (pVkCmdEndRenderPass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass2KHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
#endif

type FN_vkCmdEndRenderPass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
type PFN_vkCmdEndRenderPass2KHR = FunPtr FN_vkCmdEndRenderPass2KHR

-- | vkCmdNextSubpass2KHR - Transition to the next subpass of a render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @pSubpassBeginInfo@ is a pointer to a 'VkSubpassBeginInfoKHR'
--     structure which contains information about the subpass which is
--     about to begin rendering.
--
-- -   @pSubpassEndInfo@ is a pointer to a 'VkSubpassEndInfoKHR' structure
--     which contains information about how the previous subpass will be
--     ended.
--
-- = Description
--
-- 'vkCmdNextSubpass2KHR' is semantically identical to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass',
-- except that it is extensible, and that @contents@ is provided as part of
-- an extensible structure instead of as a flat parameter.
--
-- == Valid Usage
--
-- -   The current subpass index /must/ be less than the number of
--     subpasses in the render pass minus one
--
-- -   This command /must/ not be recorded when transform feedback is
--     active
--
-- Unresolved directive in vkCmdNextSubpass2KHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdNextSubpass2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdNextSubpass2KHR" vkCmdNextSubpass2KHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
#else
vkCmdNextSubpass2KHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
vkCmdNextSubpass2KHR deviceCmds = mkVkCmdNextSubpass2KHR (pVkCmdNextSubpass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass2KHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
#endif

type FN_vkCmdNextSubpass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
type PFN_vkCmdNextSubpass2KHR = FunPtr FN_vkCmdNextSubpass2KHR

-- | vkCreateRenderPass2KHR - Create a new render pass object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the render pass.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkRenderPassCreateInfo2KHR' structure that describes the parameters
--     of the render pass.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pRenderPass@ points to a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass' handle in which the
--     resulting render pass object is returned.
--
-- = Description
--
-- This command is functionally identical to
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass', but includes
-- extensible sub-structures that include @sType@ and @pNext@ parameters,
-- allowing them to be more easily extended.
--
-- Unresolved directive in vkCreateRenderPass2KHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateRenderPass2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateRenderPass2KHR" vkCreateRenderPass2KHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
#else
vkCreateRenderPass2KHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
vkCreateRenderPass2KHR deviceCmds = mkVkCreateRenderPass2KHR (pVkCreateRenderPass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass2KHR
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
#endif

type FN_vkCreateRenderPass2KHR = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
type PFN_vkCreateRenderPass2KHR = FunPtr FN_vkCreateRenderPass2KHR

-- No documentation found for TopLevel "VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME"
pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME = "VK_KHR_create_renderpass2"

-- No documentation found for TopLevel "VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION"
pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR"
pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR = VkStructureType 1000109000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR"
pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR = VkStructureType 1000109001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR = VkStructureType 1000109004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR = VkStructureType 1000109005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR"
pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR = VkStructureType 1000109003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR"
pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR = VkStructureType 1000109002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR = VkStructureType 1000109006
