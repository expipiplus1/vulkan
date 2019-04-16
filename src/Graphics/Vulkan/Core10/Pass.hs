{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.Pass
  ( AccessFlagBits
  , AccessFlags
  , withCStructAttachmentDescription
  , fromCStructAttachmentDescription
  , AttachmentDescription(..)
  , AttachmentDescriptionFlagBits
  , AttachmentDescriptionFlags
  , AttachmentLoadOp
  , withCStructAttachmentReference
  , fromCStructAttachmentReference
  , AttachmentReference(..)
  , AttachmentStoreOp
  , DependencyFlagBits
  , DependencyFlags
  , Framebuffer
  , FramebufferCreateFlags
  , withCStructFramebufferCreateInfo
  , fromCStructFramebufferCreateInfo
  , FramebufferCreateInfo(..)
  , PipelineBindPoint
  , RenderPassCreateFlags
  , withCStructRenderPassCreateInfo
  , fromCStructRenderPassCreateInfo
  , RenderPassCreateInfo(..)
  , withCStructSubpassDependency
  , fromCStructSubpassDependency
  , SubpassDependency(..)
  , withCStructSubpassDescription
  , fromCStructSubpassDescription
  , SubpassDescription(..)
  , SubpassDescriptionFlagBits
  , SubpassDescriptionFlags
  , createFramebuffer
  , createRenderPass
  , destroyFramebuffer
  , destroyRenderPass
  , getRenderAreaGranularity
  , withFramebuffer
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
  ( createFramebuffer
  , createRenderPass
  , destroyFramebuffer
  , destroyRenderPass
  , getRenderAreaGranularity
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkAttachmentDescription(..)
  , VkAttachmentDescriptionFlagBits(..)
  , VkAttachmentLoadOp(..)
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  , VkDependencyFlagBits(..)
  , VkFramebufferCreateFlags(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateFlags(..)
  , VkRenderPassCreateInfo(..)
  , VkSubpassDependency(..)
  , VkSubpassDescription(..)
  , VkSubpassDescriptionFlagBits(..)
  , VkFramebuffer
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
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , RenderPass
  , fromCStructExtent2D
  )
import Graphics.Vulkan.Core10.Queue
  ( PipelineStageFlags
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


-- No documentation found for TopLevel "AccessFlagBits"
type AccessFlagBits = VkAccessFlagBits
-- No documentation found for TopLevel "AccessFlags"
type AccessFlags = AccessFlagBits
-- No documentation found for TopLevel "AttachmentDescription"
data AttachmentDescription = AttachmentDescription
  { -- No documentation found for Nested "AttachmentDescription" "flags"
  vkFlags :: AttachmentDescriptionFlags
  , -- No documentation found for Nested "AttachmentDescription" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "AttachmentDescription" "samples"
  vkSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "AttachmentDescription" "loadOp"
  vkLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription" "storeOp"
  vkStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription" "stencilLoadOp"
  vkStencilLoadOp :: AttachmentLoadOp
  , -- No documentation found for Nested "AttachmentDescription" "stencilStoreOp"
  vkStencilStoreOp :: AttachmentStoreOp
  , -- No documentation found for Nested "AttachmentDescription" "initialLayout"
  vkInitialLayout :: ImageLayout
  , -- No documentation found for Nested "AttachmentDescription" "finalLayout"
  vkFinalLayout :: ImageLayout
  }
  deriving (Show, Eq)
withCStructAttachmentDescription :: AttachmentDescription -> (VkAttachmentDescription -> IO a) -> IO a
withCStructAttachmentDescription from cont = cont (VkAttachmentDescription (vkFlags (from :: AttachmentDescription)) (vkFormat (from :: AttachmentDescription)) (vkSamples (from :: AttachmentDescription)) (vkLoadOp (from :: AttachmentDescription)) (vkStoreOp (from :: AttachmentDescription)) (vkStencilLoadOp (from :: AttachmentDescription)) (vkStencilStoreOp (from :: AttachmentDescription)) (vkInitialLayout (from :: AttachmentDescription)) (vkFinalLayout (from :: AttachmentDescription)))
fromCStructAttachmentDescription :: VkAttachmentDescription -> IO AttachmentDescription
fromCStructAttachmentDescription c = AttachmentDescription <$> pure (vkFlags (c :: VkAttachmentDescription))
                                                           <*> pure (vkFormat (c :: VkAttachmentDescription))
                                                           <*> pure (vkSamples (c :: VkAttachmentDescription))
                                                           <*> pure (vkLoadOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkStoreOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkStencilLoadOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkStencilStoreOp (c :: VkAttachmentDescription))
                                                           <*> pure (vkInitialLayout (c :: VkAttachmentDescription))
                                                           <*> pure (vkFinalLayout (c :: VkAttachmentDescription))
-- No documentation found for TopLevel "AttachmentDescriptionFlagBits"
type AttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits
-- No documentation found for TopLevel "AttachmentDescriptionFlags"
type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits
-- No documentation found for TopLevel "AttachmentLoadOp"
type AttachmentLoadOp = VkAttachmentLoadOp
-- No documentation found for TopLevel "AttachmentReference"
data AttachmentReference = AttachmentReference
  { -- No documentation found for Nested "AttachmentReference" "attachment"
  vkAttachment :: Word32
  , -- No documentation found for Nested "AttachmentReference" "layout"
  vkLayout :: ImageLayout
  }
  deriving (Show, Eq)
withCStructAttachmentReference :: AttachmentReference -> (VkAttachmentReference -> IO a) -> IO a
withCStructAttachmentReference from cont = cont (VkAttachmentReference (vkAttachment (from :: AttachmentReference)) (vkLayout (from :: AttachmentReference)))
fromCStructAttachmentReference :: VkAttachmentReference -> IO AttachmentReference
fromCStructAttachmentReference c = AttachmentReference <$> pure (vkAttachment (c :: VkAttachmentReference))
                                                       <*> pure (vkLayout (c :: VkAttachmentReference))
-- No documentation found for TopLevel "AttachmentStoreOp"
type AttachmentStoreOp = VkAttachmentStoreOp
-- No documentation found for TopLevel "DependencyFlagBits"
type DependencyFlagBits = VkDependencyFlagBits
-- No documentation found for TopLevel "DependencyFlags"
type DependencyFlags = DependencyFlagBits
-- No documentation found for TopLevel "Framebuffer"
type Framebuffer = VkFramebuffer
-- No documentation found for TopLevel "FramebufferCreateFlags"
type FramebufferCreateFlags = VkFramebufferCreateFlags
-- No documentation found for TopLevel "FramebufferCreateInfo"
data FramebufferCreateInfo = FramebufferCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "FramebufferCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FramebufferCreateInfo" "flags"
  vkFlags :: FramebufferCreateFlags
  , -- No documentation found for Nested "FramebufferCreateInfo" "renderPass"
  vkRenderPass :: RenderPass
  -- Length valued member elided
  , -- No documentation found for Nested "FramebufferCreateInfo" "pAttachments"
  vkPAttachments :: Vector ImageView
  , -- No documentation found for Nested "FramebufferCreateInfo" "width"
  vkWidth :: Word32
  , -- No documentation found for Nested "FramebufferCreateInfo" "height"
  vkHeight :: Word32
  , -- No documentation found for Nested "FramebufferCreateInfo" "layers"
  vkLayers :: Word32
  }
  deriving (Show, Eq)
withCStructFramebufferCreateInfo :: FramebufferCreateInfo -> (VkFramebufferCreateInfo -> IO a) -> IO a
withCStructFramebufferCreateInfo from cont = withVec (&) (vkPAttachments (from :: FramebufferCreateInfo)) (\pAttachments -> maybeWith withSomeVkStruct (vkPNext (from :: FramebufferCreateInfo)) (\pPNext -> cont (VkFramebufferCreateInfo VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO pPNext (vkFlags (from :: FramebufferCreateInfo)) (vkRenderPass (from :: FramebufferCreateInfo)) (fromIntegral (Data.Vector.length (vkPAttachments (from :: FramebufferCreateInfo)))) pAttachments (vkWidth (from :: FramebufferCreateInfo)) (vkHeight (from :: FramebufferCreateInfo)) (vkLayers (from :: FramebufferCreateInfo)))))
fromCStructFramebufferCreateInfo :: VkFramebufferCreateInfo -> IO FramebufferCreateInfo
fromCStructFramebufferCreateInfo c = FramebufferCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFramebufferCreateInfo)))
                                                           <*> pure (vkFlags (c :: VkFramebufferCreateInfo))
                                                           <*> pure (vkRenderPass (c :: VkFramebufferCreateInfo))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkFramebufferCreateInfo))) (peekElemOff (vkPAttachments (c :: VkFramebufferCreateInfo))))
                                                           <*> pure (vkWidth (c :: VkFramebufferCreateInfo))
                                                           <*> pure (vkHeight (c :: VkFramebufferCreateInfo))
                                                           <*> pure (vkLayers (c :: VkFramebufferCreateInfo))
-- No documentation found for TopLevel "PipelineBindPoint"
type PipelineBindPoint = VkPipelineBindPoint
-- No documentation found for TopLevel "RenderPassCreateFlags"
type RenderPassCreateFlags = VkRenderPassCreateFlags
-- No documentation found for TopLevel "RenderPassCreateInfo"
data RenderPassCreateInfo = RenderPassCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassCreateInfo" "flags"
  vkFlags :: RenderPassCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo" "pAttachments"
  vkPAttachments :: Vector AttachmentDescription
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo" "pSubpasses"
  vkPSubpasses :: Vector SubpassDescription
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassCreateInfo" "pDependencies"
  vkPDependencies :: Vector SubpassDependency
  }
  deriving (Show, Eq)
withCStructRenderPassCreateInfo :: RenderPassCreateInfo -> (VkRenderPassCreateInfo -> IO a) -> IO a
withCStructRenderPassCreateInfo from cont = withVec withCStructSubpassDependency (vkPDependencies (from :: RenderPassCreateInfo)) (\pDependencies -> withVec withCStructSubpassDescription (vkPSubpasses (from :: RenderPassCreateInfo)) (\pSubpasses -> withVec withCStructAttachmentDescription (vkPAttachments (from :: RenderPassCreateInfo)) (\pAttachments -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassCreateInfo)) (\pPNext -> cont (VkRenderPassCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO pPNext (vkFlags (from :: RenderPassCreateInfo)) (fromIntegral (Data.Vector.length (vkPAttachments (from :: RenderPassCreateInfo)))) pAttachments (fromIntegral (Data.Vector.length (vkPSubpasses (from :: RenderPassCreateInfo)))) pSubpasses (fromIntegral (Data.Vector.length (vkPDependencies (from :: RenderPassCreateInfo)))) pDependencies)))))
fromCStructRenderPassCreateInfo :: VkRenderPassCreateInfo -> IO RenderPassCreateInfo
fromCStructRenderPassCreateInfo c = RenderPassCreateInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassCreateInfo)))
                                                         <*> pure (vkFlags (c :: VkRenderPassCreateInfo))
                                                         -- Length valued member elided
                                                         <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkRenderPassCreateInfo))) (((fromCStructAttachmentDescription <=<) . peekElemOff) (vkPAttachments (c :: VkRenderPassCreateInfo))))
                                                         -- Length valued member elided
                                                         <*> (Data.Vector.generateM (fromIntegral (vkSubpassCount (c :: VkRenderPassCreateInfo))) (((fromCStructSubpassDescription <=<) . peekElemOff) (vkPSubpasses (c :: VkRenderPassCreateInfo))))
                                                         -- Length valued member elided
                                                         <*> (Data.Vector.generateM (fromIntegral (vkDependencyCount (c :: VkRenderPassCreateInfo))) (((fromCStructSubpassDependency <=<) . peekElemOff) (vkPDependencies (c :: VkRenderPassCreateInfo))))
-- No documentation found for TopLevel "SubpassDependency"
data SubpassDependency = SubpassDependency
  { -- No documentation found for Nested "SubpassDependency" "srcSubpass"
  vkSrcSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency" "dstSubpass"
  vkDstSubpass :: Word32
  , -- No documentation found for Nested "SubpassDependency" "srcStageMask"
  vkSrcStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency" "dstStageMask"
  vkDstStageMask :: PipelineStageFlags
  , -- No documentation found for Nested "SubpassDependency" "srcAccessMask"
  vkSrcAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency" "dstAccessMask"
  vkDstAccessMask :: AccessFlags
  , -- No documentation found for Nested "SubpassDependency" "dependencyFlags"
  vkDependencyFlags :: DependencyFlags
  }
  deriving (Show, Eq)
withCStructSubpassDependency :: SubpassDependency -> (VkSubpassDependency -> IO a) -> IO a
withCStructSubpassDependency from cont = cont (VkSubpassDependency (vkSrcSubpass (from :: SubpassDependency)) (vkDstSubpass (from :: SubpassDependency)) (vkSrcStageMask (from :: SubpassDependency)) (vkDstStageMask (from :: SubpassDependency)) (vkSrcAccessMask (from :: SubpassDependency)) (vkDstAccessMask (from :: SubpassDependency)) (vkDependencyFlags (from :: SubpassDependency)))
fromCStructSubpassDependency :: VkSubpassDependency -> IO SubpassDependency
fromCStructSubpassDependency c = SubpassDependency <$> pure (vkSrcSubpass (c :: VkSubpassDependency))
                                                   <*> pure (vkDstSubpass (c :: VkSubpassDependency))
                                                   <*> pure (vkSrcStageMask (c :: VkSubpassDependency))
                                                   <*> pure (vkDstStageMask (c :: VkSubpassDependency))
                                                   <*> pure (vkSrcAccessMask (c :: VkSubpassDependency))
                                                   <*> pure (vkDstAccessMask (c :: VkSubpassDependency))
                                                   <*> pure (vkDependencyFlags (c :: VkSubpassDependency))
-- No documentation found for TopLevel "SubpassDescription"
data SubpassDescription = SubpassDescription
  { -- No documentation found for Nested "SubpassDescription" "flags"
  vkFlags :: SubpassDescriptionFlags
  , -- No documentation found for Nested "SubpassDescription" "pipelineBindPoint"
  vkPipelineBindPoint :: PipelineBindPoint
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription" "pInputAttachments"
  vkPInputAttachments :: Vector AttachmentReference
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription" "pColorAttachments"
  vkPColorAttachments :: Vector AttachmentReference
  , -- No documentation found for Nested "SubpassDescription" "pResolveAttachments"
  vkPResolveAttachments :: Maybe (Vector AttachmentReference)
  , -- No documentation found for Nested "SubpassDescription" "pDepthStencilAttachment"
  vkPDepthStencilAttachment :: Maybe AttachmentReference
  -- Length valued member elided
  , -- No documentation found for Nested "SubpassDescription" "pPreserveAttachments"
  vkPPreserveAttachments :: Vector Word32
  }
  deriving (Show, Eq)
withCStructSubpassDescription :: SubpassDescription -> (VkSubpassDescription -> IO a) -> IO a
withCStructSubpassDescription from cont = withVec (&) (vkPPreserveAttachments (from :: SubpassDescription)) (\pPreserveAttachments -> maybeWith (\a -> withCStructAttachmentReference a . flip with) (vkPDepthStencilAttachment (from :: SubpassDescription)) (\pDepthStencilAttachment -> maybeWith (withVec withCStructAttachmentReference) (vkPResolveAttachments (from :: SubpassDescription)) (\pResolveAttachments -> withVec withCStructAttachmentReference (vkPColorAttachments (from :: SubpassDescription)) (\pColorAttachments -> withVec withCStructAttachmentReference (vkPInputAttachments (from :: SubpassDescription)) (\pInputAttachments -> cont (VkSubpassDescription (vkFlags (from :: SubpassDescription)) (vkPipelineBindPoint (from :: SubpassDescription)) (fromIntegral (Data.Vector.length (vkPInputAttachments (from :: SubpassDescription)))) pInputAttachments (fromIntegral (minimum ([ Data.Vector.length (vkPColorAttachments (from :: SubpassDescription)) ] ++ [Data.Vector.length v | Just v <- [ (vkPResolveAttachments (from :: SubpassDescription)) ]]))) pColorAttachments pResolveAttachments pDepthStencilAttachment (fromIntegral (Data.Vector.length (vkPPreserveAttachments (from :: SubpassDescription)))) pPreserveAttachments))))))
fromCStructSubpassDescription :: VkSubpassDescription -> IO SubpassDescription
fromCStructSubpassDescription c = SubpassDescription <$> pure (vkFlags (c :: VkSubpassDescription))
                                                     <*> pure (vkPipelineBindPoint (c :: VkSubpassDescription))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkInputAttachmentCount (c :: VkSubpassDescription))) (((fromCStructAttachmentReference <=<) . peekElemOff) (vkPInputAttachments (c :: VkSubpassDescription))))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkColorAttachmentCount (c :: VkSubpassDescription))) (((fromCStructAttachmentReference <=<) . peekElemOff) (vkPColorAttachments (c :: VkSubpassDescription))))
                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkColorAttachmentCount (c :: VkSubpassDescription))) (((fromCStructAttachmentReference <=<) . peekElemOff) p)) (vkPResolveAttachments (c :: VkSubpassDescription))
                                                     <*> maybePeek (fromCStructAttachmentReference <=< peek) (vkPDepthStencilAttachment (c :: VkSubpassDescription))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkPreserveAttachmentCount (c :: VkSubpassDescription))) (peekElemOff (vkPPreserveAttachments (c :: VkSubpassDescription))))
-- No documentation found for TopLevel "SubpassDescriptionFlagBits"
type SubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits
-- No documentation found for TopLevel "SubpassDescriptionFlags"
type SubpassDescriptionFlags = SubpassDescriptionFlagBits

-- | Wrapper for 'vkCreateFramebuffer'
createFramebuffer :: Device ->  FramebufferCreateInfo ->  Maybe AllocationCallbacks ->  IO ( Framebuffer )
createFramebuffer = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pFramebuffer -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructFramebufferCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createFramebuffer commandTable device pCreateInfo pAllocator pFramebuffer >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFramebuffer)))))

-- | Wrapper for 'vkCreateRenderPass'
createRenderPass :: Device ->  RenderPassCreateInfo ->  Maybe AllocationCallbacks ->  IO ( RenderPass )
createRenderPass = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pRenderPass -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructRenderPassCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createRenderPass commandTable device pCreateInfo pAllocator pRenderPass >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pRenderPass)))))

-- | Wrapper for 'vkDestroyFramebuffer'
destroyFramebuffer :: Device ->  Framebuffer ->  Maybe AllocationCallbacks ->  IO ()
destroyFramebuffer = \(Device device commandTable) -> \framebuffer -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyFramebuffer commandTable device framebuffer pAllocator *> (pure ()))

-- | Wrapper for 'vkDestroyRenderPass'
destroyRenderPass :: Device ->  RenderPass ->  Maybe AllocationCallbacks ->  IO ()
destroyRenderPass = \(Device device commandTable) -> \renderPass -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyRenderPass commandTable device renderPass pAllocator *> (pure ()))

-- | Wrapper for 'vkGetRenderAreaGranularity'
getRenderAreaGranularity :: Device ->  RenderPass ->  IO (Extent2D)
getRenderAreaGranularity = \(Device device commandTable) -> \renderPass -> alloca (\pGranularity -> Graphics.Vulkan.C.Dynamic.getRenderAreaGranularity commandTable device renderPass pGranularity *> ((fromCStructExtent2D <=< peek) pGranularity))
withFramebuffer :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withFramebuffer createInfo allocationCallbacks =
  bracket
    (vkCreateFramebuffer createInfo allocationCallbacks)
    (`vkDestroyFramebuffer` allocationCallbacks)
