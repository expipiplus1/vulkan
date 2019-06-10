{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  AttachmentDescription2KHR(..)
  , 
  AttachmentReference2KHR(..)
  , RenderPassCreateInfo2KHR(..)
  , SubpassBeginInfoKHR(..)
  , SubpassDependency2KHR(..)
  , SubpassDescription2KHR(..)
  , SubpassEndInfoKHR(..)
#endif
  , cmdBeginRenderPass2KHR
  , cmdEndRenderPass2KHR
  , cmdNextSubpass2KHR
  , createRenderPass2KHR
  , pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  , pattern KHR_CREATE_RENDERPASS_2_SPEC_VERSION
  , pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.Int
  ( Int32
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( vkCmdBeginRenderPass2KHR
  , vkCmdEndRenderPass2KHR
  , vkCmdNextSubpass2KHR
  , vkCreateRenderPass2KHR
  , pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  , pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( RenderPassBeginInfo(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( SubpassContents
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( SampleCountFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( AttachmentDescriptionFlags
  , DependencyFlags
  , PipelineBindPoint
  , RenderPassCreateFlags
  , SubpassDescriptionFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( AccessFlags
  , AttachmentLoadOp
  , AttachmentStoreOp
  )
#endif
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( PipelineStageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAttachmentDescription2KHR"
data AttachmentDescription2KHR = AttachmentDescription2KHR
  { -- No documentation found for Nested "AttachmentDescription2KHR" "pNext"
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

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAttachmentReference2KHR"
data AttachmentReference2KHR = AttachmentReference2KHR
  { -- No documentation found for Nested "AttachmentReference2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AttachmentReference2KHR" "attachment"
  attachment :: Word32
  , -- No documentation found for Nested "AttachmentReference2KHR" "layout"
  layout :: ImageLayout
  , -- No documentation found for Nested "AttachmentReference2KHR" "aspectMask"
  aspectMask :: ImageAspectFlags
  }
  deriving (Show, Eq)

instance Zero AttachmentReference2KHR where
  zero = AttachmentReference2KHR Nothing
                                 zero
                                 zero
                                 zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassCreateInfo2KHR"
data RenderPassCreateInfo2KHR = RenderPassCreateInfo2KHR
  { -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "flags"
  flags :: RenderPassCreateFlags
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pAttachments"
  attachments :: Vector AttachmentDescription2KHR
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pSubpasses"
  subpasses :: Vector SubpassDescription2KHR
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pDependencies"
  dependencies :: Vector SubpassDependency2KHR
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pCorrelatedViewMasks"
  correlatedViewMasks :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero RenderPassCreateInfo2KHR where
  zero = RenderPassCreateInfo2KHR Nothing
                                  zero
                                  mempty
                                  mempty
                                  mempty
                                  mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSubpassBeginInfoKHR"
data SubpassBeginInfoKHR = SubpassBeginInfoKHR
  { -- No documentation found for Nested "SubpassBeginInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassBeginInfoKHR" "contents"
  contents :: SubpassContents
  }
  deriving (Show, Eq)

instance Zero SubpassBeginInfoKHR where
  zero = SubpassBeginInfoKHR Nothing
                             zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSubpassDependency2KHR"
data SubpassDependency2KHR = SubpassDependency2KHR
  { -- No documentation found for Nested "SubpassDependency2KHR" "pNext"
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

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSubpassDescription2KHR"
data SubpassDescription2KHR = SubpassDescription2KHR
  { -- No documentation found for Nested "SubpassDescription2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDescription2KHR" "flags"
  flags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "SubpassDescription2KHR" "pipelineBindPoint"
  pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "SubpassDescription2KHR" "viewMask"
  viewMask :: Word32
  , -- No documentation found for Nested "SubpassDescription2KHR" "pInputAttachments"
  inputAttachments :: Vector AttachmentReference2KHR
  , -- No documentation found for Nested "SubpassDescription2KHR" "pColorAttachments"
  colorAttachments :: Vector AttachmentReference2KHR
  , -- No documentation found for Nested "SubpassDescription2KHR" "pResolveAttachments"
  resolveAttachments :: Either Word32 (Vector AttachmentReference2KHR)
  , -- No documentation found for Nested "SubpassDescription2KHR" "pDepthStencilAttachment"
  depthStencilAttachment :: Maybe AttachmentReference2KHR
  , -- No documentation found for Nested "SubpassDescription2KHR" "pPreserveAttachments"
  preserveAttachments :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero SubpassDescription2KHR where
  zero = SubpassDescription2KHR Nothing
                                zero
                                zero
                                zero
                                mempty
                                mempty
                                (Left 0)
                                Nothing
                                mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSubpassEndInfoKHR"
data SubpassEndInfoKHR = SubpassEndInfoKHR
  { -- No documentation found for Nested "SubpassEndInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  }
  deriving (Show, Eq)

instance Zero SubpassEndInfoKHR where
  zero = SubpassEndInfoKHR Nothing

#endif


-- No documentation found for TopLevel "vkCmdBeginRenderPass2KHR"
cmdBeginRenderPass2KHR :: CommandBuffer ->  RenderPassBeginInfo ->  SubpassBeginInfoKHR ->  IO ()
cmdBeginRenderPass2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndRenderPass2KHR"
cmdEndRenderPass2KHR :: CommandBuffer ->  SubpassEndInfoKHR ->  IO ()
cmdEndRenderPass2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdNextSubpass2KHR"
cmdNextSubpass2KHR :: CommandBuffer ->  SubpassBeginInfoKHR ->  SubpassEndInfoKHR ->  IO ()
cmdNextSubpass2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateRenderPass2KHR"
createRenderPass2KHR :: Device ->  RenderPassCreateInfo2KHR ->  Maybe AllocationCallbacks ->  IO (RenderPass)
createRenderPass2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME"
pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME = VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION"
pattern KHR_CREATE_RENDERPASS_2_SPEC_VERSION :: Integral a => a
pattern KHR_CREATE_RENDERPASS_2_SPEC_VERSION = VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION
