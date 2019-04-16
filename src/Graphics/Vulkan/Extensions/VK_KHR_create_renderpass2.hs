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
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdBeginRenderPass2KHR
  , cmdEndRenderPass2KHR
  , cmdNextSubpass2KHR
  , createRenderPass2KHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentDescription2KHR(..)
  , VkAttachmentReference2KHR(..)
  , VkRenderPassCreateInfo2KHR(..)
  , VkSubpassBeginInfoKHR(..)
  , VkSubpassDependency2KHR(..)
  , VkSubpassDescription2KHR(..)
  , VkSubpassEndInfoKHR(..)
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


-- No documentation found for TopLevel "AttachmentDescription2KHR"
data AttachmentDescription2KHR = AttachmentDescription2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "AttachmentDescription2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AttachmentDescription2KHR" "flags"
  vkFlags :: AttachmentDescriptionFlags
  , -- No documentation found for Nested "AttachmentDescription2KHR" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "AttachmentDescription2KHR" "samples"
  vkSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "AttachmentDescription2KHR" "loadOp"
  vkLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "storeOp"
  vkStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "stencilLoadOp"
  vkStencilLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "stencilStoreOp"
  vkStencilStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription2KHR" "initialLayout"
  vkInitialLayout :: ImageLayout
  , -- No documentation found for Nested "AttachmentDescription2KHR" "finalLayout"
  vkFinalLayout :: ImageLayout
  }
  deriving (Show, Eq)
withCStructAttachmentDescription2KHR :: AttachmentDescription2KHR -> (VkAttachmentDescription2KHR -> IO a) -> IO a
withCStructAttachmentDescription2KHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: AttachmentDescription2KHR)) (\pPNext -> cont (VkAttachmentDescription2KHR VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR pPNext (vkFlags (from :: AttachmentDescription2KHR)) (vkFormat (from :: AttachmentDescription2KHR)) (vkSamples (from :: AttachmentDescription2KHR)) (vkLoadOp (from :: AttachmentDescription2KHR)) (vkStoreOp (from :: AttachmentDescription2KHR)) (vkStencilLoadOp (from :: AttachmentDescription2KHR)) (vkStencilStoreOp (from :: AttachmentDescription2KHR)) (vkInitialLayout (from :: AttachmentDescription2KHR)) (vkFinalLayout (from :: AttachmentDescription2KHR))))
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
-- No documentation found for TopLevel "AttachmentReference2KHR"
data AttachmentReference2KHR = AttachmentReference2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "AttachmentReference2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AttachmentReference2KHR" "attachment"
  vkAttachment :: Word32
  , -- No documentation found for Nested "AttachmentReference2KHR" "layout"
  vkLayout :: ImageLayout
  , -- No documentation found for Nested "AttachmentReference2KHR" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  }
  deriving (Show, Eq)
withCStructAttachmentReference2KHR :: AttachmentReference2KHR -> (VkAttachmentReference2KHR -> IO a) -> IO a
withCStructAttachmentReference2KHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: AttachmentReference2KHR)) (\pPNext -> cont (VkAttachmentReference2KHR VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR pPNext (vkAttachment (from :: AttachmentReference2KHR)) (vkLayout (from :: AttachmentReference2KHR)) (vkAspectMask (from :: AttachmentReference2KHR))))
fromCStructAttachmentReference2KHR :: VkAttachmentReference2KHR -> IO AttachmentReference2KHR
fromCStructAttachmentReference2KHR c = AttachmentReference2KHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAttachmentReference2KHR)))
                                                               <*> pure (vkAttachment (c :: VkAttachmentReference2KHR))
                                                               <*> pure (vkLayout (c :: VkAttachmentReference2KHR))
                                                               <*> pure (vkAspectMask (c :: VkAttachmentReference2KHR))
-- No documentation found for TopLevel "RenderPassCreateInfo2KHR"
data RenderPassCreateInfo2KHR = RenderPassCreateInfo2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "flags"
  vkFlags :: RenderPassCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pAttachments"
  vkPAttachments :: Vector AttachmentDescription2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pSubpasses"
  vkPSubpasses :: Vector SubpassDescription2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pDependencies"
  vkPDependencies :: Vector SubpassDependency2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo2KHR" "pCorrelatedViewMasks"
  vkPCorrelatedViewMasks :: Vector Word32
  }
  deriving (Show, Eq)
withCStructRenderPassCreateInfo2KHR :: RenderPassCreateInfo2KHR -> (VkRenderPassCreateInfo2KHR -> IO a) -> IO a
withCStructRenderPassCreateInfo2KHR from cont = withVec (&) (vkPCorrelatedViewMasks (from :: RenderPassCreateInfo2KHR)) (\pCorrelatedViewMasks -> withVec withCStructSubpassDependency2KHR (vkPDependencies (from :: RenderPassCreateInfo2KHR)) (\pDependencies -> withVec withCStructSubpassDescription2KHR (vkPSubpasses (from :: RenderPassCreateInfo2KHR)) (\pSubpasses -> withVec withCStructAttachmentDescription2KHR (vkPAttachments (from :: RenderPassCreateInfo2KHR)) (\pAttachments -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassCreateInfo2KHR)) (\pPNext -> cont (VkRenderPassCreateInfo2KHR VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR pPNext (vkFlags (from :: RenderPassCreateInfo2KHR)) (fromIntegral (Data.Vector.length (vkPAttachments (from :: RenderPassCreateInfo2KHR)))) pAttachments (fromIntegral (Data.Vector.length (vkPSubpasses (from :: RenderPassCreateInfo2KHR)))) pSubpasses (fromIntegral (Data.Vector.length (vkPDependencies (from :: RenderPassCreateInfo2KHR)))) pDependencies (fromIntegral (Data.Vector.length (vkPCorrelatedViewMasks (from :: RenderPassCreateInfo2KHR)))) pCorrelatedViewMasks))))))
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
-- No documentation found for TopLevel "SubpassBeginInfoKHR"
data SubpassBeginInfoKHR = SubpassBeginInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SubpassBeginInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassBeginInfoKHR" "contents"
  vkContents :: SubpassContents
  }
  deriving (Show, Eq)
withCStructSubpassBeginInfoKHR :: SubpassBeginInfoKHR -> (VkSubpassBeginInfoKHR -> IO a) -> IO a
withCStructSubpassBeginInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SubpassBeginInfoKHR)) (\pPNext -> cont (VkSubpassBeginInfoKHR VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR pPNext (vkContents (from :: SubpassBeginInfoKHR))))
fromCStructSubpassBeginInfoKHR :: VkSubpassBeginInfoKHR -> IO SubpassBeginInfoKHR
fromCStructSubpassBeginInfoKHR c = SubpassBeginInfoKHR <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassBeginInfoKHR)))
                                                       <*> pure (vkContents (c :: VkSubpassBeginInfoKHR))
-- No documentation found for TopLevel "SubpassDependency2KHR"
data SubpassDependency2KHR = SubpassDependency2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SubpassDependency2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDependency2KHR" "srcSubpass"
  vkSrcSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency2KHR" "dstSubpass"
  vkDstSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency2KHR" "srcStageMask"
  vkSrcStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "dstStageMask"
  vkDstStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "srcAccessMask"
  vkSrcAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "dstAccessMask"
  vkDstAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "dependencyFlags"
  vkDependencyFlags :: DependencyFlags
  , -- No documentation found for Nested "SubpassDependency2KHR" "viewOffset"
  vkViewOffset :: Int32
  }
  deriving (Show, Eq)
withCStructSubpassDependency2KHR :: SubpassDependency2KHR -> (VkSubpassDependency2KHR -> IO a) -> IO a
withCStructSubpassDependency2KHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SubpassDependency2KHR)) (\pPNext -> cont (VkSubpassDependency2KHR VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR pPNext (vkSrcSubpass (from :: SubpassDependency2KHR)) (vkDstSubpass (from :: SubpassDependency2KHR)) (vkSrcStageMask (from :: SubpassDependency2KHR)) (vkDstStageMask (from :: SubpassDependency2KHR)) (vkSrcAccessMask (from :: SubpassDependency2KHR)) (vkDstAccessMask (from :: SubpassDependency2KHR)) (vkDependencyFlags (from :: SubpassDependency2KHR)) (vkViewOffset (from :: SubpassDependency2KHR))))
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
-- No documentation found for TopLevel "SubpassDescription2KHR"
data SubpassDescription2KHR = SubpassDescription2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SubpassDescription2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubpassDescription2KHR" "flags"
  vkFlags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "SubpassDescription2KHR" "pipelineBindPoint"
  vkPipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "SubpassDescription2KHR" "viewMask"
  vkViewMask :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription2KHR" "pInputAttachments"
  vkPInputAttachments :: Vector AttachmentReference2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription2KHR" "pColorAttachments"
  vkPColorAttachments :: Vector AttachmentReference2KHR
  , -- No documentation found for Nested "SubpassDescription2KHR" "pResolveAttachments"
  vkPResolveAttachments :: Maybe (Vector AttachmentReference2KHR)
  , -- No documentation found for Nested "SubpassDescription2KHR" "pDepthStencilAttachment"
  vkPDepthStencilAttachment :: Maybe AttachmentReference2KHR
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription2KHR" "pPreserveAttachments"
  vkPPreserveAttachments :: Vector Word32
  }
  deriving (Show, Eq)
withCStructSubpassDescription2KHR :: SubpassDescription2KHR -> (VkSubpassDescription2KHR -> IO a) -> IO a
withCStructSubpassDescription2KHR from cont = withVec (&) (vkPPreserveAttachments (from :: SubpassDescription2KHR)) (\pPreserveAttachments -> maybeWith (\a -> withCStructAttachmentReference2KHR a . flip with) (vkPDepthStencilAttachment (from :: SubpassDescription2KHR)) (\pDepthStencilAttachment -> maybeWith (withVec withCStructAttachmentReference2KHR) (vkPResolveAttachments (from :: SubpassDescription2KHR)) (\pResolveAttachments -> withVec withCStructAttachmentReference2KHR (vkPColorAttachments (from :: SubpassDescription2KHR)) (\pColorAttachments -> withVec withCStructAttachmentReference2KHR (vkPInputAttachments (from :: SubpassDescription2KHR)) (\pInputAttachments -> maybeWith withSomeVkStruct (vkPNext (from :: SubpassDescription2KHR)) (\pPNext -> cont (VkSubpassDescription2KHR VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR pPNext (vkFlags (from :: SubpassDescription2KHR)) (vkPipelineBindPoint (from :: SubpassDescription2KHR)) (vkViewMask (from :: SubpassDescription2KHR)) (fromIntegral (Data.Vector.length (vkPInputAttachments (from :: SubpassDescription2KHR)))) pInputAttachments (fromIntegral (minimum ([Data.Vector.length (vkPColorAttachments (from :: SubpassDescription2KHR))] ++ [Data.Vector.length v | Just v <- [(vkPResolveAttachments (from :: SubpassDescription2KHR))]]))) pColorAttachments pResolveAttachments pDepthStencilAttachment (fromIntegral (Data.Vector.length (vkPPreserveAttachments (from :: SubpassDescription2KHR)))) pPreserveAttachments)))))))
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
-- No documentation found for TopLevel "SubpassEndInfoKHR"
data SubpassEndInfoKHR = SubpassEndInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SubpassEndInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  }
  deriving (Show, Eq)
withCStructSubpassEndInfoKHR :: SubpassEndInfoKHR -> (VkSubpassEndInfoKHR -> IO a) -> IO a
withCStructSubpassEndInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SubpassEndInfoKHR)) (\pPNext -> cont (VkSubpassEndInfoKHR VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR pPNext))
fromCStructSubpassEndInfoKHR :: VkSubpassEndInfoKHR -> IO SubpassEndInfoKHR
fromCStructSubpassEndInfoKHR c = SubpassEndInfoKHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubpassEndInfoKHR)))

-- | Wrapper for vkCmdBeginRenderPass2KHR
cmdBeginRenderPass2KHR :: CommandBuffer ->  RenderPassBeginInfo ->  SubpassBeginInfoKHR ->  IO ()
cmdBeginRenderPass2KHR = \(CommandBuffer commandBuffer commandTable) -> \renderPassBegin -> \subpassBeginInfo -> (\a -> withCStructSubpassBeginInfoKHR a . flip with) subpassBeginInfo (\pSubpassBeginInfo -> (\a -> withCStructRenderPassBeginInfo a . flip with) renderPassBegin (\pRenderPassBegin -> Graphics.Vulkan.C.Dynamic.cmdBeginRenderPass2KHR commandTable commandBuffer pRenderPassBegin pSubpassBeginInfo *> (pure ())))

-- | Wrapper for vkCmdEndRenderPass2KHR
cmdEndRenderPass2KHR :: CommandBuffer ->  SubpassEndInfoKHR ->  IO ()
cmdEndRenderPass2KHR = \(CommandBuffer commandBuffer commandTable) -> \subpassEndInfo -> (\a -> withCStructSubpassEndInfoKHR a . flip with) subpassEndInfo (\pSubpassEndInfo -> Graphics.Vulkan.C.Dynamic.cmdEndRenderPass2KHR commandTable commandBuffer pSubpassEndInfo *> (pure ()))

-- | Wrapper for vkCmdNextSubpass2KHR
cmdNextSubpass2KHR :: CommandBuffer ->  SubpassBeginInfoKHR ->  SubpassEndInfoKHR ->  IO ()
cmdNextSubpass2KHR = \(CommandBuffer commandBuffer commandTable) -> \subpassBeginInfo -> \subpassEndInfo -> (\a -> withCStructSubpassEndInfoKHR a . flip with) subpassEndInfo (\pSubpassEndInfo -> (\a -> withCStructSubpassBeginInfoKHR a . flip with) subpassBeginInfo (\pSubpassBeginInfo -> Graphics.Vulkan.C.Dynamic.cmdNextSubpass2KHR commandTable commandBuffer pSubpassBeginInfo pSubpassEndInfo *> (pure ())))

-- | Wrapper for vkCreateRenderPass2KHR
createRenderPass2KHR :: Device ->  RenderPassCreateInfo2KHR ->  Maybe AllocationCallbacks ->  IO (RenderPass)
createRenderPass2KHR = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pRenderPass -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructRenderPassCreateInfo2KHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createRenderPass2KHR commandTable device pCreateInfo pAllocator pRenderPass >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pRenderPass)))))
