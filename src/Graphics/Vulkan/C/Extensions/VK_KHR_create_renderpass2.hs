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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdBeginRenderPass2KHR
#endif
  , FN_vkCmdBeginRenderPass2KHR
  , PFN_vkCmdBeginRenderPass2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdEndRenderPass2KHR
#endif
  , FN_vkCmdEndRenderPass2KHR
  , PFN_vkCmdEndRenderPass2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdNextSubpass2KHR
#endif
  , FN_vkCmdNextSubpass2KHR
  , PFN_vkCmdNextSubpass2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateRenderPass2KHR
#endif
  , FN_vkCreateRenderPass2KHR
  , PFN_vkCreateRenderPass2KHR
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkAttachmentDescription2KHR"
data VkAttachmentDescription2KHR = VkAttachmentDescription2KHR
  { -- No documentation found for Nested "VkAttachmentDescription2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "flags"
  vkFlags :: VkAttachmentDescriptionFlags
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "samples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "loadOp"
  vkLoadOp :: VkAttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "storeOp"
  vkStoreOp :: VkAttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "stencilLoadOp"
  vkStencilLoadOp :: VkAttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "stencilStoreOp"
  vkStencilStoreOp :: VkAttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "initialLayout"
  vkInitialLayout :: VkImageLayout
  , -- No documentation found for Nested "VkAttachmentDescription2KHR" "finalLayout"
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
-- No documentation found for TopLevel "VkAttachmentReference2KHR"
data VkAttachmentReference2KHR = VkAttachmentReference2KHR
  { -- No documentation found for Nested "VkAttachmentReference2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAttachmentReference2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAttachmentReference2KHR" "attachment"
  vkAttachment :: Word32
  , -- No documentation found for Nested "VkAttachmentReference2KHR" "layout"
  vkLayout :: VkImageLayout
  , -- No documentation found for Nested "VkAttachmentReference2KHR" "aspectMask"
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
-- No documentation found for TopLevel "VkRenderPassCreateInfo2KHR"
data VkRenderPassCreateInfo2KHR = VkRenderPassCreateInfo2KHR
  { -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "flags"
  vkFlags :: VkRenderPassCreateFlags
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "attachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "pAttachments"
  vkPAttachments :: Ptr VkAttachmentDescription2KHR
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "subpassCount"
  vkSubpassCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "pSubpasses"
  vkPSubpasses :: Ptr VkSubpassDescription2KHR
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "dependencyCount"
  vkDependencyCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "pDependencies"
  vkPDependencies :: Ptr VkSubpassDependency2KHR
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "correlatedViewMaskCount"
  vkCorrelatedViewMaskCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo2KHR" "pCorrelatedViewMasks"
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
-- No documentation found for TopLevel "VkSubpassBeginInfoKHR"
data VkSubpassBeginInfoKHR = VkSubpassBeginInfoKHR
  { -- No documentation found for Nested "VkSubpassBeginInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSubpassBeginInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSubpassBeginInfoKHR" "contents"
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
-- No documentation found for TopLevel "VkSubpassDependency2KHR"
data VkSubpassDependency2KHR = VkSubpassDependency2KHR
  { -- No documentation found for Nested "VkSubpassDependency2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "srcSubpass"
  vkSrcSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "dstSubpass"
  vkDstSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "srcStageMask"
  vkSrcStageMask :: VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "dstStageMask"
  vkDstStageMask :: VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "srcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "dstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "dependencyFlags"
  vkDependencyFlags :: VkDependencyFlags
  , -- No documentation found for Nested "VkSubpassDependency2KHR" "viewOffset"
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
-- No documentation found for TopLevel "VkSubpassDescription2KHR"
data VkSubpassDescription2KHR = VkSubpassDescription2KHR
  { -- No documentation found for Nested "VkSubpassDescription2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "flags"
  vkFlags :: VkSubpassDescriptionFlags
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pipelineBindPoint"
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "viewMask"
  vkViewMask :: Word32
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "inputAttachmentCount"
  vkInputAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pInputAttachments"
  vkPInputAttachments :: Ptr VkAttachmentReference2KHR
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "colorAttachmentCount"
  vkColorAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pColorAttachments"
  vkPColorAttachments :: Ptr VkAttachmentReference2KHR
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pResolveAttachments"
  vkPResolveAttachments :: Ptr VkAttachmentReference2KHR
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pDepthStencilAttachment"
  vkPDepthStencilAttachment :: Ptr VkAttachmentReference2KHR
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "preserveAttachmentCount"
  vkPreserveAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription2KHR" "pPreserveAttachments"
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
-- No documentation found for TopLevel "VkSubpassEndInfoKHR"
data VkSubpassEndInfoKHR = VkSubpassEndInfoKHR
  { -- No documentation found for Nested "VkSubpassEndInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSubpassEndInfoKHR" "pNext"
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdBeginRenderPass2KHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginRenderPass2KHR" vkCmdBeginRenderPass2KHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()

#endif
type FN_vkCmdBeginRenderPass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()
type PFN_vkCmdBeginRenderPass2KHR = FunPtr FN_vkCmdBeginRenderPass2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdEndRenderPass2KHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndRenderPass2KHR" vkCmdEndRenderPass2KHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()

#endif
type FN_vkCmdEndRenderPass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
type PFN_vkCmdEndRenderPass2KHR = FunPtr FN_vkCmdEndRenderPass2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdNextSubpass2KHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdNextSubpass2KHR" vkCmdNextSubpass2KHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()

#endif
type FN_vkCmdNextSubpass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
type PFN_vkCmdNextSubpass2KHR = FunPtr FN_vkCmdNextSubpass2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateRenderPass2KHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateRenderPass2KHR" vkCreateRenderPass2KHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult

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
