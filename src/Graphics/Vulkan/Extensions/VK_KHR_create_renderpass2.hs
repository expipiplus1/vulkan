{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( withCStructAttachmentDescription2KHR
  , fromCStructAttachmentDescription2KHR
  , AttachmentDescription2KHR(..)
  , withCStructAttachmentReference2KHR
  , fromCStructAttachmentReference2KHR
  , AttachmentReference2KHR(..)
  , withCStructRenderPassCreateInfo2KHR
  , fromCStructRenderPassCreateInfo2KHR
  , RenderPassCreateInfo2KHR(..)
  , withCStructSubpassBeginInfoKHR
  , fromCStructSubpassBeginInfoKHR
  , SubpassBeginInfoKHR(..)
  , withCStructSubpassDependency2KHR
  , fromCStructSubpassDependency2KHR
  , SubpassDependency2KHR(..)
  , withCStructSubpassDescription2KHR
  , fromCStructSubpassDescription2KHR
  , SubpassDescription2KHR(..)
  , withCStructSubpassEndInfoKHR
  , fromCStructSubpassEndInfoKHR
  , SubpassEndInfoKHR(..)
  , cmdBeginRenderPass2KHR
  , cmdEndRenderPass2KHR
  , cmdNextSubpass2KHR
  , createRenderPass2KHR
  , pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION
  , pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
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
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentDescription2KHR(..)
  , VkAttachmentReference2KHR(..)
  , VkRenderPassCreateInfo2KHR(..)
  , VkSubpassBeginInfoKHR(..)
  , VkSubpassDependency2KHR(..)
  , VkSubpassDescription2KHR(..)
  , VkSubpassEndInfoKHR(..)
  , vkCmdBeginRenderPass2KHR
  , vkCmdEndRenderPass2KHR
  , vkCmdNextSubpass2KHR
  , vkCreateRenderPass2KHR
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( RenderPassBeginInfo(..)
  , SubpassContents
  , withCStructRenderPassBeginInfo
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , SampleCountFlagBits
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.Pass
  ( AccessFlags
  , AttachmentDescriptionFlags
  , AttachmentLoadOp
  , AttachmentStoreOp
  , DependencyFlags
  , PipelineBindPoint
  , RenderPassCreateFlags
  , SubpassDescriptionFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , PipelineStageFlags
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  , pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION
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
data AttachmentDescription2KHR = AttachmentDescription2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "AttachmentDescription2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AttachmentDescription2KHR" "flags"
  flags :: AttachmentDescriptionFlags
  , -- No documentation found for Nested "AttachmentDescription2KHR" "format"
  format :: Format
  , -- No documentation found for Nested "AttachmentDescription2KHR" "samples"
  samples :: SampleCountFlagBits
  , -- No documentation found for Nested "AttachmentDescription2KHR" "loadOp"
  loadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "storeOp"
  storeOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "stencilLoadOp"
  stencilLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "stencilStoreOp"
  stencilStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "initialLayout"
  initialLayout :: ImageLayout
  , -- No documentation found for Nested "AttachmentDescription2KHR" "finalLayout"
  finalLayout :: ImageLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAttachmentDescription2KHR' and
-- marshal a 'AttachmentDescription2KHR' into it. The 'VkAttachmentDescription2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAttachmentDescription2KHR :: AttachmentDescription2KHR -> (VkAttachmentDescription2KHR -> IO a) -> IO a
withCStructAttachmentDescription2KHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AttachmentDescription2KHR)) (\pPNext -> cont (VkAttachmentDescription2KHR VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR pPNext (flags (marshalled :: AttachmentDescription2KHR)) (format (marshalled :: AttachmentDescription2KHR)) (samples (marshalled :: AttachmentDescription2KHR)) (loadOp (marshalled :: AttachmentDescription2KHR)) (storeOp (marshalled :: AttachmentDescription2KHR)) (stencilLoadOp (marshalled :: AttachmentDescription2KHR)) (stencilStoreOp (marshalled :: AttachmentDescription2KHR)) (initialLayout (marshalled :: AttachmentDescription2KHR)) (finalLayout (marshalled :: AttachmentDescription2KHR))))

-- | A function to read a 'VkAttachmentDescription2KHR' and all additional
-- structures in the pointer chain into a 'AttachmentDescription2KHR'.
fromCStructAttachmentDescription2KHR :: VkAttachmentDescription2KHR -> IO AttachmentDescription2KHR
fromCStructAttachmentDescription2KHR c = AttachmentDescription2KHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAttachmentDescription2KHR)))
                                                                   <*> pure (vkFlags (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkFormat (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkSamples (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkLoadOp (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkStoreOp (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkStencilLoadOp (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkStencilStoreOp (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkInitialLayout (c :: VkAttachmentDescription2KHR))
                                                                   <*> pure (vkFinalLayout (c :: VkAttachmentDescription2KHR))

instance Zero AttachmentDescription2KHR where
  zero = AttachmentDescription2KHR Nothing
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
data AttachmentReference2KHR = AttachmentReference2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "AttachmentReference2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AttachmentReference2KHR" "attachment"
  attachment :: Word32
  , -- No documentation found for Nested "AttachmentReference2KHR" "layout"
  layout :: ImageLayout
  , -- No documentation found for Nested "AttachmentReference2KHR" "aspectMask"
  aspectMask :: ImageAspectFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAttachmentReference2KHR' and
-- marshal a 'AttachmentReference2KHR' into it. The 'VkAttachmentReference2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAttachmentReference2KHR :: AttachmentReference2KHR -> (VkAttachmentReference2KHR -> IO a) -> IO a
withCStructAttachmentReference2KHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AttachmentReference2KHR)) (\pPNext -> cont (VkAttachmentReference2KHR VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR pPNext (attachment (marshalled :: AttachmentReference2KHR)) (layout (marshalled :: AttachmentReference2KHR)) (aspectMask (marshalled :: AttachmentReference2KHR))))

-- | A function to read a 'VkAttachmentReference2KHR' and all additional
-- structures in the pointer chain into a 'AttachmentReference2KHR'.
fromCStructAttachmentReference2KHR :: VkAttachmentReference2KHR -> IO AttachmentReference2KHR
fromCStructAttachmentReference2KHR c = AttachmentReference2KHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAttachmentReference2KHR)))
                                                               <*> pure (vkAttachment (c :: VkAttachmentReference2KHR))
                                                               <*> pure (vkLayout (c :: VkAttachmentReference2KHR))
                                                               <*> pure (vkAspectMask (c :: VkAttachmentReference2KHR))

instance Zero AttachmentReference2KHR where
  zero = AttachmentReference2KHR Nothing
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
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassDescription2KHR'::@viewMask@
-- member of any element of @pSubpasses@ is not zero, /multiview/
-- functionality is considered to be enabled for this render pass.
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
--     same subpass), the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkAttachmentDescription2KHR'
--     structures describing them /must/ include
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
-- -   If the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassDescription2KHR'::@viewMask@
--     member of all elements of @pSubpasses@ is @0@,
--     @correlatedViewMaskCount@ /must/ be @0@
--
-- -   The
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassDescription2KHR'::@viewMask@
--     member of all elements of @pSubpasses@ /must/ either all be @0@, or
--     all not be @0@
--
-- -   If the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassDescription2KHR'::@viewMask@
--     member of all elements of @pSubpasses@ is @0@, the @dependencyFlags@
--     member of any element of @pDependencies@ /must/ not include
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
data RenderPassCreateInfo2KHR = RenderPassCreateInfo2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "flags"
  flags :: RenderPassCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pAttachments"
  attachments :: Vector AttachmentDescription2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pSubpasses"
  subpasses :: Vector SubpassDescription2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pDependencies"
  dependencies :: Vector SubpassDependency2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pCorrelatedViewMasks"
  correlatedViewMasks :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassCreateInfo2KHR' and
-- marshal a 'RenderPassCreateInfo2KHR' into it. The 'VkRenderPassCreateInfo2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassCreateInfo2KHR :: RenderPassCreateInfo2KHR -> (VkRenderPassCreateInfo2KHR -> IO a) -> IO a
withCStructRenderPassCreateInfo2KHR marshalled cont = withVec (&) (correlatedViewMasks (marshalled :: RenderPassCreateInfo2KHR)) (\pPCorrelatedViewMasks -> withVec withCStructSubpassDependency2KHR (dependencies (marshalled :: RenderPassCreateInfo2KHR)) (\pPDependencies -> withVec withCStructSubpassDescription2KHR (subpasses (marshalled :: RenderPassCreateInfo2KHR)) (\pPSubpasses -> withVec withCStructAttachmentDescription2KHR (attachments (marshalled :: RenderPassCreateInfo2KHR)) (\pPAttachments -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassCreateInfo2KHR)) (\pPNext -> cont (VkRenderPassCreateInfo2KHR VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR pPNext (flags (marshalled :: RenderPassCreateInfo2KHR)) (fromIntegral (Data.Vector.length (attachments (marshalled :: RenderPassCreateInfo2KHR)))) pPAttachments (fromIntegral (Data.Vector.length (subpasses (marshalled :: RenderPassCreateInfo2KHR)))) pPSubpasses (fromIntegral (Data.Vector.length (dependencies (marshalled :: RenderPassCreateInfo2KHR)))) pPDependencies (fromIntegral (Data.Vector.length (correlatedViewMasks (marshalled :: RenderPassCreateInfo2KHR)))) pPCorrelatedViewMasks))))))

-- | A function to read a 'VkRenderPassCreateInfo2KHR' and all additional
-- structures in the pointer chain into a 'RenderPassCreateInfo2KHR'.
fromCStructRenderPassCreateInfo2KHR :: VkRenderPassCreateInfo2KHR -> IO RenderPassCreateInfo2KHR
fromCStructRenderPassCreateInfo2KHR c = RenderPassCreateInfo2KHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassCreateInfo2KHR)))
                                                                 <*> pure (vkFlags (c :: VkRenderPassCreateInfo2KHR))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkRenderPassCreateInfo2KHR))) (((fromCStructAttachmentDescription2KHR <=<) . peekElemOff) (vkPAttachments (c :: VkRenderPassCreateInfo2KHR))))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkSubpassCount (c :: VkRenderPassCreateInfo2KHR))) (((fromCStructSubpassDescription2KHR <=<) . peekElemOff) (vkPSubpasses (c :: VkRenderPassCreateInfo2KHR))))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkDependencyCount (c :: VkRenderPassCreateInfo2KHR))) (((fromCStructSubpassDependency2KHR <=<) . peekElemOff) (vkPDependencies (c :: VkRenderPassCreateInfo2KHR))))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkCorrelatedViewMaskCount (c :: VkRenderPassCreateInfo2KHR))) (peekElemOff (vkPCorrelatedViewMasks (c :: VkRenderPassCreateInfo2KHR))))

instance Zero RenderPassCreateInfo2KHR where
  zero = RenderPassCreateInfo2KHR Nothing
                                  zero
                                  Data.Vector.empty
                                  Data.Vector.empty
                                  Data.Vector.empty
                                  Data.Vector.empty



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
data SubpassBeginInfoKHR = SubpassBeginInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SubpassBeginInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassBeginInfoKHR" "contents"
  contents :: SubpassContents
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassBeginInfoKHR' and
-- marshal a 'SubpassBeginInfoKHR' into it. The 'VkSubpassBeginInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassBeginInfoKHR :: SubpassBeginInfoKHR -> (VkSubpassBeginInfoKHR -> IO a) -> IO a
withCStructSubpassBeginInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SubpassBeginInfoKHR)) (\pPNext -> cont (VkSubpassBeginInfoKHR VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR pPNext (contents (marshalled :: SubpassBeginInfoKHR))))

-- | A function to read a 'VkSubpassBeginInfoKHR' and all additional
-- structures in the pointer chain into a 'SubpassBeginInfoKHR'.
fromCStructSubpassBeginInfoKHR :: VkSubpassBeginInfoKHR -> IO SubpassBeginInfoKHR
fromCStructSubpassBeginInfoKHR c = SubpassBeginInfoKHR <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassBeginInfoKHR)))
                                                       <*> pure (vkContents (c :: VkSubpassBeginInfoKHR))

instance Zero SubpassBeginInfoKHR where
  zero = SubpassBeginInfoKHR Nothing
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
data SubpassDependency2KHR = SubpassDependency2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "SubpassDependency2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDependency2KHR" "srcSubpass"
  srcSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency2KHR" "dstSubpass"
  dstSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency2KHR" "srcStageMask"
  srcStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "dstStageMask"
  dstStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "dstAccessMask"
  dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "dependencyFlags"
  dependencyFlags :: DependencyFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "viewOffset"
  viewOffset :: Int32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassDependency2KHR' and
-- marshal a 'SubpassDependency2KHR' into it. The 'VkSubpassDependency2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassDependency2KHR :: SubpassDependency2KHR -> (VkSubpassDependency2KHR -> IO a) -> IO a
withCStructSubpassDependency2KHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SubpassDependency2KHR)) (\pPNext -> cont (VkSubpassDependency2KHR VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR pPNext (srcSubpass (marshalled :: SubpassDependency2KHR)) (dstSubpass (marshalled :: SubpassDependency2KHR)) (srcStageMask (marshalled :: SubpassDependency2KHR)) (dstStageMask (marshalled :: SubpassDependency2KHR)) (srcAccessMask (marshalled :: SubpassDependency2KHR)) (dstAccessMask (marshalled :: SubpassDependency2KHR)) (dependencyFlags (marshalled :: SubpassDependency2KHR)) (viewOffset (marshalled :: SubpassDependency2KHR))))

-- | A function to read a 'VkSubpassDependency2KHR' and all additional
-- structures in the pointer chain into a 'SubpassDependency2KHR'.
fromCStructSubpassDependency2KHR :: VkSubpassDependency2KHR -> IO SubpassDependency2KHR
fromCStructSubpassDependency2KHR c = SubpassDependency2KHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassDependency2KHR)))
                                                           <*> pure (vkSrcSubpass (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkDstSubpass (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkSrcStageMask (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkDstStageMask (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkSrcAccessMask (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkDstAccessMask (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkDependencyFlags (c :: VkSubpassDependency2KHR))
                                                           <*> pure (vkViewOffset (c :: VkSubpassDependency2KHR))

instance Zero SubpassDependency2KHR where
  zero = SubpassDependency2KHR Nothing
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
data SubpassDescription2KHR = SubpassDescription2KHR
  { -- Univalued member elided
  -- No documentation found for Nested "SubpassDescription2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDescription2KHR" "flags"
  flags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "SubpassDescription2KHR" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "SubpassDescription2KHR" "viewMask"
  viewMask :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription2KHR" "pInputAttachments"
  inputAttachments :: Vector AttachmentReference2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription2KHR" "pColorAttachments"
  colorAttachments :: Vector AttachmentReference2KHR
  , -- No documentation found for Nested "SubpassDescription2KHR" "pResolveAttachments"
  resolveAttachments :: Maybe (Vector AttachmentReference2KHR)
  , -- No documentation found for Nested "SubpassDescription2KHR" "pDepthStencilAttachment"
  depthStencilAttachment :: Maybe AttachmentReference2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription2KHR" "pPreserveAttachments"
  preserveAttachments :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassDescription2KHR' and
-- marshal a 'SubpassDescription2KHR' into it. The 'VkSubpassDescription2KHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassDescription2KHR :: SubpassDescription2KHR -> (VkSubpassDescription2KHR -> IO a) -> IO a
withCStructSubpassDescription2KHR marshalled cont = withVec (&) (preserveAttachments (marshalled :: SubpassDescription2KHR)) (\pPPreserveAttachments -> maybeWith (\a -> withCStructAttachmentReference2KHR a . flip with) (depthStencilAttachment (marshalled :: SubpassDescription2KHR)) (\pPDepthStencilAttachment -> maybeWith (withVec withCStructAttachmentReference2KHR) (resolveAttachments (marshalled :: SubpassDescription2KHR)) (\pPResolveAttachments -> withVec withCStructAttachmentReference2KHR (colorAttachments (marshalled :: SubpassDescription2KHR)) (\pPColorAttachments -> withVec withCStructAttachmentReference2KHR (inputAttachments (marshalled :: SubpassDescription2KHR)) (\pPInputAttachments -> maybeWith withSomeVkStruct (next (marshalled :: SubpassDescription2KHR)) (\pPNext -> cont (VkSubpassDescription2KHR VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR pPNext (flags (marshalled :: SubpassDescription2KHR)) (pipelineBindPoint (marshalled :: SubpassDescription2KHR)) (viewMask (marshalled :: SubpassDescription2KHR)) (fromIntegral (Data.Vector.length (inputAttachments (marshalled :: SubpassDescription2KHR)))) pPInputAttachments (fromIntegral (minimum ([Data.Vector.length (colorAttachments (marshalled :: SubpassDescription2KHR))] ++ [Data.Vector.length v | Just v <- [(resolveAttachments (marshalled :: SubpassDescription2KHR))]]))) pPColorAttachments pPResolveAttachments pPDepthStencilAttachment (fromIntegral (Data.Vector.length (preserveAttachments (marshalled :: SubpassDescription2KHR)))) pPPreserveAttachments)))))))

-- | A function to read a 'VkSubpassDescription2KHR' and all additional
-- structures in the pointer chain into a 'SubpassDescription2KHR'.
fromCStructSubpassDescription2KHR :: VkSubpassDescription2KHR -> IO SubpassDescription2KHR
fromCStructSubpassDescription2KHR c = SubpassDescription2KHR <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassDescription2KHR)))
                                                             <*> pure (vkFlags (c :: VkSubpassDescription2KHR))
                                                             <*> pure (vkPipelineBindPoint (c :: VkSubpassDescription2KHR))
                                                             <*> pure (vkViewMask (c :: VkSubpassDescription2KHR))
                                                             -- Length valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkInputAttachmentCount (c :: VkSubpassDescription2KHR))) (((fromCStructAttachmentReference2KHR <=<) . peekElemOff) (vkPInputAttachments (c :: VkSubpassDescription2KHR))))
                                                             -- Length valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkColorAttachmentCount (c :: VkSubpassDescription2KHR))) (((fromCStructAttachmentReference2KHR <=<) . peekElemOff) (vkPColorAttachments (c :: VkSubpassDescription2KHR))))
                                                             <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkColorAttachmentCount (c :: VkSubpassDescription2KHR))) (((fromCStructAttachmentReference2KHR <=<) . peekElemOff) p)) (vkPResolveAttachments (c :: VkSubpassDescription2KHR))
                                                             <*> maybePeek (fromCStructAttachmentReference2KHR <=< peek) (vkPDepthStencilAttachment (c :: VkSubpassDescription2KHR))
                                                             -- Length valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkPreserveAttachmentCount (c :: VkSubpassDescription2KHR))) (peekElemOff (vkPPreserveAttachments (c :: VkSubpassDescription2KHR))))

instance Zero SubpassDescription2KHR where
  zero = SubpassDescription2KHR Nothing
                                zero
                                zero
                                zero
                                Data.Vector.empty
                                Data.Vector.empty
                                Nothing
                                Nothing
                                Data.Vector.empty



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
data SubpassEndInfoKHR = SubpassEndInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SubpassEndInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassEndInfoKHR' and
-- marshal a 'SubpassEndInfoKHR' into it. The 'VkSubpassEndInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassEndInfoKHR :: SubpassEndInfoKHR -> (VkSubpassEndInfoKHR -> IO a) -> IO a
withCStructSubpassEndInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SubpassEndInfoKHR)) (\pPNext -> cont (VkSubpassEndInfoKHR VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR pPNext))

-- | A function to read a 'VkSubpassEndInfoKHR' and all additional
-- structures in the pointer chain into a 'SubpassEndInfoKHR'.
fromCStructSubpassEndInfoKHR :: VkSubpassEndInfoKHR -> IO SubpassEndInfoKHR
fromCStructSubpassEndInfoKHR c = SubpassEndInfoKHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassEndInfoKHR)))

instance Zero SubpassEndInfoKHR where
  zero = SubpassEndInfoKHR Nothing



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
-- -   @pSubpassBeginInfo@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassBeginInfoKHR'
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
cmdBeginRenderPass2KHR :: CommandBuffer ->  RenderPassBeginInfo ->  SubpassBeginInfoKHR ->  IO ()
cmdBeginRenderPass2KHR = \(CommandBuffer commandBuffer' commandTable) -> \renderPassBegin' -> \subpassBeginInfo' -> (\marshalled -> withCStructSubpassBeginInfoKHR marshalled . flip with) subpassBeginInfo' (\pSubpassBeginInfo' -> (\marshalled -> withCStructRenderPassBeginInfo marshalled . flip with) renderPassBegin' (\pRenderPassBegin' -> vkCmdBeginRenderPass2KHR commandTable commandBuffer' pRenderPassBegin' pSubpassBeginInfo' *> (pure ())))


-- | vkCmdEndRenderPass2KHR - End the current render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to end the current
--     render pass instance.
--
-- -   @pSubpassEndInfo@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassEndInfoKHR'
--     structure which contains information about how the previous subpass
--     will be ended.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCmdEndRenderPass2KHR'
-- is semantically identical to
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
cmdEndRenderPass2KHR :: CommandBuffer ->  SubpassEndInfoKHR ->  IO ()
cmdEndRenderPass2KHR = \(CommandBuffer commandBuffer' commandTable) -> \subpassEndInfo' -> (\marshalled -> withCStructSubpassEndInfoKHR marshalled . flip with) subpassEndInfo' (\pSubpassEndInfo' -> vkCmdEndRenderPass2KHR commandTable commandBuffer' pSubpassEndInfo' *> (pure ()))


-- | vkCmdNextSubpass2KHR - Transition to the next subpass of a render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @pSubpassBeginInfo@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassBeginInfoKHR'
--     structure which contains information about the subpass which is
--     about to begin rendering.
--
-- -   @pSubpassEndInfo@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassEndInfoKHR'
--     structure which contains information about how the previous subpass
--     will be ended.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCmdNextSubpass2KHR'
-- is semantically identical to
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
cmdNextSubpass2KHR :: CommandBuffer ->  SubpassBeginInfoKHR ->  SubpassEndInfoKHR ->  IO ()
cmdNextSubpass2KHR = \(CommandBuffer commandBuffer' commandTable) -> \subpassBeginInfo' -> \subpassEndInfo' -> (\marshalled -> withCStructSubpassEndInfoKHR marshalled . flip with) subpassEndInfo' (\pSubpassEndInfo' -> (\marshalled -> withCStructSubpassBeginInfoKHR marshalled . flip with) subpassBeginInfo' (\pSubpassBeginInfo' -> vkCmdNextSubpass2KHR commandTable commandBuffer' pSubpassBeginInfo' pSubpassEndInfo' *> (pure ())))


-- | vkCreateRenderPass2KHR - Create a new render pass object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the render pass.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkRenderPassCreateInfo2KHR'
--     structure that describes the parameters of the render pass.
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
createRenderPass2KHR :: Device ->  RenderPassCreateInfo2KHR ->  Maybe AllocationCallbacks ->  IO (RenderPass)
createRenderPass2KHR = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pRenderPass' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructRenderPassCreateInfo2KHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateRenderPass2KHR commandTable device' pCreateInfo' pAllocator pRenderPass' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pRenderPass')))))
