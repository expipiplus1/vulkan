{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( withCStructImageViewUsageCreateInfo
  , fromCStructImageViewUsageCreateInfo
  , ImageViewUsageCreateInfo(..)
  , withCStructInputAttachmentAspectReference
  , fromCStructInputAttachmentAspectReference
  , InputAttachmentAspectReference(..)
  , withCStructPhysicalDevicePointClippingProperties
  , fromCStructPhysicalDevicePointClippingProperties
  , PhysicalDevicePointClippingProperties(..)
  , withCStructPipelineTessellationDomainOriginStateCreateInfo
  , fromCStructPipelineTessellationDomainOriginStateCreateInfo
  , PipelineTessellationDomainOriginStateCreateInfo(..)
  , PointClippingBehavior
  , PointClippingBehaviorKHR
  , withCStructRenderPassInputAttachmentAspectCreateInfo
  , fromCStructRenderPassInputAttachmentAspectCreateInfo
  , RenderPassInputAttachmentAspectCreateInfo(..)
  , TessellationDomainOrigin
  , TessellationDomainOriginKHR
  , pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  ) where

import Control.Monad
  ( (<=<)
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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkInputAttachmentAspectReference(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkPointClippingBehavior(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , VkTessellationDomainOrigin(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  )


-- No documentation found for TopLevel "ImageViewUsageCreateInfo"
data ImageViewUsageCreateInfo = ImageViewUsageCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageViewUsageCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewUsageCreateInfo" "usage"
  vkUsage :: ImageUsageFlags
  }
  deriving (Show, Eq)
withCStructImageViewUsageCreateInfo :: ImageViewUsageCreateInfo -> (VkImageViewUsageCreateInfo -> IO a) -> IO a
withCStructImageViewUsageCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageViewUsageCreateInfo)) (\pPNext -> cont (VkImageViewUsageCreateInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO pPNext (vkUsage (from :: ImageViewUsageCreateInfo))))
fromCStructImageViewUsageCreateInfo :: VkImageViewUsageCreateInfo -> IO ImageViewUsageCreateInfo
fromCStructImageViewUsageCreateInfo c = ImageViewUsageCreateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewUsageCreateInfo)))
                                                                 <*> pure (vkUsage (c :: VkImageViewUsageCreateInfo))
instance Zero ImageViewUsageCreateInfo where
  zero = ImageViewUsageCreateInfo Nothing
                                  zero
-- No documentation found for TopLevel "InputAttachmentAspectReference"
data InputAttachmentAspectReference = InputAttachmentAspectReference
  { -- No documentation found for Nested "InputAttachmentAspectReference" "subpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "InputAttachmentAspectReference" "inputAttachmentIndex"
  vkInputAttachmentIndex :: Word32
  , -- No documentation found for Nested "InputAttachmentAspectReference" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  }
  deriving (Show, Eq)
withCStructInputAttachmentAspectReference :: InputAttachmentAspectReference -> (VkInputAttachmentAspectReference -> IO a) -> IO a
withCStructInputAttachmentAspectReference from cont = cont (VkInputAttachmentAspectReference (vkSubpass (from :: InputAttachmentAspectReference)) (vkInputAttachmentIndex (from :: InputAttachmentAspectReference)) (vkAspectMask (from :: InputAttachmentAspectReference)))
fromCStructInputAttachmentAspectReference :: VkInputAttachmentAspectReference -> IO InputAttachmentAspectReference
fromCStructInputAttachmentAspectReference c = InputAttachmentAspectReference <$> pure (vkSubpass (c :: VkInputAttachmentAspectReference))
                                                                             <*> pure (vkInputAttachmentIndex (c :: VkInputAttachmentAspectReference))
                                                                             <*> pure (vkAspectMask (c :: VkInputAttachmentAspectReference))
instance Zero InputAttachmentAspectReference where
  zero = InputAttachmentAspectReference zero
                                        zero
                                        zero
-- No documentation found for TopLevel "PhysicalDevicePointClippingProperties"
data PhysicalDevicePointClippingProperties = PhysicalDevicePointClippingProperties
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDevicePointClippingProperties" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePointClippingProperties" "pointClippingBehavior"
  vkPointClippingBehavior :: PointClippingBehavior
  }
  deriving (Show, Eq)
withCStructPhysicalDevicePointClippingProperties :: PhysicalDevicePointClippingProperties -> (VkPhysicalDevicePointClippingProperties -> IO a) -> IO a
withCStructPhysicalDevicePointClippingProperties from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDevicePointClippingProperties)) (\pPNext -> cont (VkPhysicalDevicePointClippingProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES pPNext (vkPointClippingBehavior (from :: PhysicalDevicePointClippingProperties))))
fromCStructPhysicalDevicePointClippingProperties :: VkPhysicalDevicePointClippingProperties -> IO PhysicalDevicePointClippingProperties
fromCStructPhysicalDevicePointClippingProperties c = PhysicalDevicePointClippingProperties <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevicePointClippingProperties)))
                                                                                           <*> pure (vkPointClippingBehavior (c :: VkPhysicalDevicePointClippingProperties))
instance Zero PhysicalDevicePointClippingProperties where
  zero = PhysicalDevicePointClippingProperties Nothing
                                               zero
-- No documentation found for TopLevel "PipelineTessellationDomainOriginStateCreateInfo"
data PipelineTessellationDomainOriginStateCreateInfo = PipelineTessellationDomainOriginStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineTessellationDomainOriginStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineTessellationDomainOriginStateCreateInfo" "domainOrigin"
  vkDomainOrigin :: TessellationDomainOrigin
  }
  deriving (Show, Eq)
withCStructPipelineTessellationDomainOriginStateCreateInfo :: PipelineTessellationDomainOriginStateCreateInfo -> (VkPipelineTessellationDomainOriginStateCreateInfo -> IO a) -> IO a
withCStructPipelineTessellationDomainOriginStateCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineTessellationDomainOriginStateCreateInfo)) (\pPNext -> cont (VkPipelineTessellationDomainOriginStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO pPNext (vkDomainOrigin (from :: PipelineTessellationDomainOriginStateCreateInfo))))
fromCStructPipelineTessellationDomainOriginStateCreateInfo :: VkPipelineTessellationDomainOriginStateCreateInfo -> IO PipelineTessellationDomainOriginStateCreateInfo
fromCStructPipelineTessellationDomainOriginStateCreateInfo c = PipelineTessellationDomainOriginStateCreateInfo <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineTessellationDomainOriginStateCreateInfo)))
                                                                                                               <*> pure (vkDomainOrigin (c :: VkPipelineTessellationDomainOriginStateCreateInfo))
instance Zero PipelineTessellationDomainOriginStateCreateInfo where
  zero = PipelineTessellationDomainOriginStateCreateInfo Nothing
                                                         zero
-- No documentation found for TopLevel "PointClippingBehavior"
type PointClippingBehavior = VkPointClippingBehavior
-- No documentation found for TopLevel "PointClippingBehaviorKHR"
type PointClippingBehaviorKHR = PointClippingBehavior
-- No documentation found for TopLevel "RenderPassInputAttachmentAspectCreateInfo"
data RenderPassInputAttachmentAspectCreateInfo = RenderPassInputAttachmentAspectCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassInputAttachmentAspectCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassInputAttachmentAspectCreateInfo" "pAspectReferences"
  vkPAspectReferences :: Vector InputAttachmentAspectReference
  }
  deriving (Show, Eq)
withCStructRenderPassInputAttachmentAspectCreateInfo :: RenderPassInputAttachmentAspectCreateInfo -> (VkRenderPassInputAttachmentAspectCreateInfo -> IO a) -> IO a
withCStructRenderPassInputAttachmentAspectCreateInfo from cont = withVec withCStructInputAttachmentAspectReference (vkPAspectReferences (from :: RenderPassInputAttachmentAspectCreateInfo)) (\pAspectReferences -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassInputAttachmentAspectCreateInfo)) (\pPNext -> cont (VkRenderPassInputAttachmentAspectCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO pPNext (fromIntegral (Data.Vector.length (vkPAspectReferences (from :: RenderPassInputAttachmentAspectCreateInfo)))) pAspectReferences)))
fromCStructRenderPassInputAttachmentAspectCreateInfo :: VkRenderPassInputAttachmentAspectCreateInfo -> IO RenderPassInputAttachmentAspectCreateInfo
fromCStructRenderPassInputAttachmentAspectCreateInfo c = RenderPassInputAttachmentAspectCreateInfo <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassInputAttachmentAspectCreateInfo)))
                                                                                                   -- Length valued member elided
                                                                                                   <*> (Data.Vector.generateM (fromIntegral (vkAspectReferenceCount (c :: VkRenderPassInputAttachmentAspectCreateInfo))) (((fromCStructInputAttachmentAspectReference <=<) . peekElemOff) (vkPAspectReferences (c :: VkRenderPassInputAttachmentAspectCreateInfo))))
instance Zero RenderPassInputAttachmentAspectCreateInfo where
  zero = RenderPassInputAttachmentAspectCreateInfo Nothing
                                                   Data.Vector.empty
-- No documentation found for TopLevel "TessellationDomainOrigin"
type TessellationDomainOrigin = VkTessellationDomainOrigin
-- No documentation found for TopLevel "TessellationDomainOriginKHR"
type TessellationDomainOriginKHR = TessellationDomainOrigin
