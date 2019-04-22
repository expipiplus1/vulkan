{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( withCStructBindSparseInfo
  , fromCStructBindSparseInfo
  , BindSparseInfo(..)
  , ImageAspectFlagBits
  , pattern IMAGE_ASPECT_COLOR_BIT
  , pattern IMAGE_ASPECT_DEPTH_BIT
  , pattern IMAGE_ASPECT_STENCIL_BIT
  , pattern IMAGE_ASPECT_METADATA_BIT
  , pattern IMAGE_ASPECT_PLANE_0_BIT
  , pattern IMAGE_ASPECT_PLANE_1_BIT
  , pattern IMAGE_ASPECT_PLANE_2_BIT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  , ImageAspectFlags
  , withCStructImageSubresource
  , fromCStructImageSubresource
  , ImageSubresource(..)
  , withCStructOffset3D
  , fromCStructOffset3D
  , Offset3D(..)
  , withCStructSparseBufferMemoryBindInfo
  , fromCStructSparseBufferMemoryBindInfo
  , SparseBufferMemoryBindInfo(..)
  , SparseImageFormatFlagBits
  , pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
  , pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
  , pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
  , SparseImageFormatFlags
  , withCStructSparseImageFormatProperties
  , fromCStructSparseImageFormatProperties
  , SparseImageFormatProperties(..)
  , withCStructSparseImageMemoryBind
  , fromCStructSparseImageMemoryBind
  , SparseImageMemoryBind(..)
  , withCStructSparseImageMemoryBindInfo
  , fromCStructSparseImageMemoryBindInfo
  , SparseImageMemoryBindInfo(..)
  , withCStructSparseImageMemoryRequirements
  , fromCStructSparseImageMemoryRequirements
  , SparseImageMemoryRequirements(..)
  , withCStructSparseImageOpaqueMemoryBindInfo
  , fromCStructSparseImageOpaqueMemoryBindInfo
  , SparseImageOpaqueMemoryBindInfo(..)
  , withCStructSparseMemoryBind
  , fromCStructSparseMemoryBind
  , SparseMemoryBind(..)
  , SparseMemoryBindFlagBits
  , pattern SPARSE_MEMORY_BIND_METADATA_BIT
  , SparseMemoryBindFlags
  , getNumImageSparseMemoryRequirements
  , getImageSparseMemoryRequirements
  , getAllImageSparseMemoryRequirements
  , getNumPhysicalDeviceSparseImageFormatProperties
  , getPhysicalDeviceSparseImageFormatProperties
  , getAllPhysicalDeviceSparseImageFormatProperties
  , queueBindSparse
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
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageAspectFlagBits(..)
  , VkImageSubresource(..)
  , VkOffset3D(..)
  , VkSparseBufferMemoryBindInfo(..)
  , VkSparseImageFormatFlagBits(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryBind(..)
  , VkSparseImageMemoryBindInfo(..)
  , VkSparseImageMemoryRequirements(..)
  , VkSparseImageOpaqueMemoryBindInfo(..)
  , VkSparseMemoryBind(..)
  , VkSparseMemoryBindFlagBits(..)
  , vkGetImageSparseMemoryRequirements
  , vkGetPhysicalDeviceSparseImageFormatProperties
  , vkQueueBindSparse
  , pattern VK_IMAGE_ASPECT_COLOR_BIT
  , pattern VK_IMAGE_ASPECT_DEPTH_BIT
  , pattern VK_IMAGE_ASPECT_METADATA_BIT
  , pattern VK_IMAGE_ASPECT_STENCIL_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
  , pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
  , pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( pattern VK_IMAGE_ASPECT_PLANE_0_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_1_BIT
  , pattern VK_IMAGE_ASPECT_PLANE_2_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , Extent3D(..)
  , PhysicalDevice(..)
  , DeviceSize
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , SampleCountFlagBits
  , fromCStructExtent3D
  , withCStructExtent3D
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import Graphics.Vulkan.Core10.Queue
  ( Queue(..)
  , Fence
  , Semaphore
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



-- | VkBindSparseInfo - Structure specifying a sparse binding operation
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_BIND_SPARSE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupBindSparseInfo'
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid
--     'Graphics.Vulkan.C.Core10.Queue.VkSemaphore' handles
--
-- -   If @bufferBindCount@ is not @0@, @pBufferBinds@ /must/ be a valid
--     pointer to an array of @bufferBindCount@ valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseBufferMemoryBindInfo'
--     structures
--
-- -   If @imageOpaqueBindCount@ is not @0@, @pImageOpaqueBinds@ /must/ be
--     a valid pointer to an array of @imageOpaqueBindCount@ valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageOpaqueMemoryBindInfo'
--     structures
--
-- -   If @imageBindCount@ is not @0@, @pImageBinds@ /must/ be a valid
--     pointer to an array of @imageBindCount@ valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBindInfo'
--     structures
--
-- -   If @signalSemaphoreCount@ is not @0@, @pSignalSemaphores@ /must/ be
--     a valid pointer to an array of @signalSemaphoreCount@ valid
--     'Graphics.Vulkan.C.Core10.Queue.VkSemaphore' handles
--
-- -   Both of the elements of @pSignalSemaphores@, and the elements of
--     @pWaitSemaphores@ that are valid handles /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseBufferMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageOpaqueMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse'
data BindSparseInfo = BindSparseInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindSparseInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pWaitSemaphores"
  waitSemaphores :: Vector Semaphore
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pBufferBinds"
  bufferBinds :: Vector SparseBufferMemoryBindInfo
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pImageOpaqueBinds"
  imageOpaqueBinds :: Vector SparseImageOpaqueMemoryBindInfo
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pImageBinds"
  imageBinds :: Vector SparseImageMemoryBindInfo
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pSignalSemaphores"
  signalSemaphores :: Vector Semaphore
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindSparseInfo' and
-- marshal a 'BindSparseInfo' into it. The 'VkBindSparseInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindSparseInfo :: BindSparseInfo -> (VkBindSparseInfo -> IO a) -> IO a
withCStructBindSparseInfo marshalled cont = withVec (&) (signalSemaphores (marshalled :: BindSparseInfo)) (\pPSignalSemaphores -> withVec withCStructSparseImageMemoryBindInfo (imageBinds (marshalled :: BindSparseInfo)) (\pPImageBinds -> withVec withCStructSparseImageOpaqueMemoryBindInfo (imageOpaqueBinds (marshalled :: BindSparseInfo)) (\pPImageOpaqueBinds -> withVec withCStructSparseBufferMemoryBindInfo (bufferBinds (marshalled :: BindSparseInfo)) (\pPBufferBinds -> withVec (&) (waitSemaphores (marshalled :: BindSparseInfo)) (\pPWaitSemaphores -> maybeWith withSomeVkStruct (next (marshalled :: BindSparseInfo)) (\pPNext -> cont (VkBindSparseInfo VK_STRUCTURE_TYPE_BIND_SPARSE_INFO pPNext (fromIntegral (Data.Vector.length (waitSemaphores (marshalled :: BindSparseInfo)))) pPWaitSemaphores (fromIntegral (Data.Vector.length (bufferBinds (marshalled :: BindSparseInfo)))) pPBufferBinds (fromIntegral (Data.Vector.length (imageOpaqueBinds (marshalled :: BindSparseInfo)))) pPImageOpaqueBinds (fromIntegral (Data.Vector.length (imageBinds (marshalled :: BindSparseInfo)))) pPImageBinds (fromIntegral (Data.Vector.length (signalSemaphores (marshalled :: BindSparseInfo)))) pPSignalSemaphores)))))))

-- | A function to read a 'VkBindSparseInfo' and all additional
-- structures in the pointer chain into a 'BindSparseInfo'.
fromCStructBindSparseInfo :: VkBindSparseInfo -> IO BindSparseInfo
fromCStructBindSparseInfo c = BindSparseInfo <$> -- Univalued Member elided
                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindSparseInfo)))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkBindSparseInfo))) (peekElemOff (vkPWaitSemaphores (c :: VkBindSparseInfo))))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkBufferBindCount (c :: VkBindSparseInfo))) (((fromCStructSparseBufferMemoryBindInfo <=<) . peekElemOff) (vkPBufferBinds (c :: VkBindSparseInfo))))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkImageOpaqueBindCount (c :: VkBindSparseInfo))) (((fromCStructSparseImageOpaqueMemoryBindInfo <=<) . peekElemOff) (vkPImageOpaqueBinds (c :: VkBindSparseInfo))))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkImageBindCount (c :: VkBindSparseInfo))) (((fromCStructSparseImageMemoryBindInfo <=<) . peekElemOff) (vkPImageBinds (c :: VkBindSparseInfo))))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkSignalSemaphoreCount (c :: VkBindSparseInfo))) (peekElemOff (vkPSignalSemaphores (c :: VkBindSparseInfo))))

instance Zero BindSparseInfo where
  zero = BindSparseInfo Nothing
                        Data.Vector.empty
                        Data.Vector.empty
                        Data.Vector.empty
                        Data.Vector.empty
                        Data.Vector.empty


-- | VkImageAspectFlagBits - Bitmask specifying which aspects of an image are
-- included in a view
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
type ImageAspectFlagBits = VkImageAspectFlagBits


{-# complete IMAGE_ASPECT_COLOR_BIT, IMAGE_ASPECT_DEPTH_BIT, IMAGE_ASPECT_STENCIL_BIT, IMAGE_ASPECT_METADATA_BIT, IMAGE_ASPECT_PLANE_0_BIT, IMAGE_ASPECT_PLANE_1_BIT, IMAGE_ASPECT_PLANE_2_BIT, IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT, IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT, IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT, IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT :: ImageAspectFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT'
-- specifies the color aspect.
pattern IMAGE_ASPECT_COLOR_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_COLOR_BIT = VK_IMAGE_ASPECT_COLOR_BIT


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- specifies the depth aspect.
pattern IMAGE_ASPECT_DEPTH_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_DEPTH_BIT = VK_IMAGE_ASPECT_DEPTH_BIT


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
-- specifies the stencil aspect.
pattern IMAGE_ASPECT_STENCIL_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_STENCIL_BIT = VK_IMAGE_ASPECT_STENCIL_BIT


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_METADATA_BIT'
-- specifies the metadata aspect, used for sparse
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory sparse resource>
-- operations.
pattern IMAGE_ASPECT_METADATA_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_METADATA_BIT = VK_IMAGE_ASPECT_METADATA_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_PLANE_0_BIT"
pattern IMAGE_ASPECT_PLANE_0_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_PLANE_0_BIT = VK_IMAGE_ASPECT_PLANE_0_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_PLANE_1_BIT"
pattern IMAGE_ASPECT_PLANE_1_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_PLANE_1_BIT = VK_IMAGE_ASPECT_PLANE_1_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_PLANE_2_BIT"
pattern IMAGE_ASPECT_PLANE_2_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_PLANE_2_BIT = VK_IMAGE_ASPECT_PLANE_2_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT = VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT = VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT = VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT = VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT

-- | VkImageAspectFlags - Bitmask of VkImageAspectFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkAttachmentReference2KHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearAttachment',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkInputAttachmentAspectReference',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
type ImageAspectFlags = ImageAspectFlagBits


-- | VkImageSubresource - Structure specifying an image subresource
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout'
data ImageSubresource = ImageSubresource
  { -- No documentation found for Nested "ImageSubresource" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresource" "mipLevel"
  mipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresource" "arrayLayer"
  arrayLayer :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageSubresource' and
-- marshal a 'ImageSubresource' into it. The 'VkImageSubresource' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageSubresource :: ImageSubresource -> (VkImageSubresource -> IO a) -> IO a
withCStructImageSubresource marshalled cont = cont (VkImageSubresource (aspectMask (marshalled :: ImageSubresource)) (mipLevel (marshalled :: ImageSubresource)) (arrayLayer (marshalled :: ImageSubresource)))

-- | A function to read a 'VkImageSubresource' and all additional
-- structures in the pointer chain into a 'ImageSubresource'.
fromCStructImageSubresource :: VkImageSubresource -> IO ImageSubresource
fromCStructImageSubresource c = ImageSubresource <$> pure (vkAspectMask (c :: VkImageSubresource))
                                                 <*> pure (vkMipLevel (c :: VkImageSubresource))
                                                 <*> pure (vkArrayLayer (c :: VkImageSubresource))

instance Zero ImageSubresource where
  zero = ImageSubresource zero
                          zero
                          zero



-- | VkOffset3D - Structure specifying a three-dimensional offset
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageBlit',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind'
data Offset3D = Offset3D
  { -- No documentation found for Nested "Offset3D" "x"
  x :: Int32
  , -- No documentation found for Nested "Offset3D" "y"
  y :: Int32
  , -- No documentation found for Nested "Offset3D" "z"
  z :: Int32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkOffset3D' and
-- marshal a 'Offset3D' into it. The 'VkOffset3D' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructOffset3D :: Offset3D -> (VkOffset3D -> IO a) -> IO a
withCStructOffset3D marshalled cont = cont (VkOffset3D (x (marshalled :: Offset3D)) (y (marshalled :: Offset3D)) (z (marshalled :: Offset3D)))

-- | A function to read a 'VkOffset3D' and all additional
-- structures in the pointer chain into a 'Offset3D'.
fromCStructOffset3D :: VkOffset3D -> IO Offset3D
fromCStructOffset3D c = Offset3D <$> pure (vkX (c :: VkOffset3D))
                                 <*> pure (vkY (c :: VkOffset3D))
                                 <*> pure (vkZ (c :: VkOffset3D))

instance Zero Offset3D where
  zero = Offset3D zero
                  zero
                  zero



-- | VkSparseBufferMemoryBindInfo - Structure specifying a sparse buffer
-- memory bind operation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind'
data SparseBufferMemoryBindInfo = SparseBufferMemoryBindInfo
  { -- No documentation found for Nested "SparseBufferMemoryBindInfo" "buffer"
  buffer :: Buffer
  -- Length valued member elided
  , -- No documentation found for Nested "SparseBufferMemoryBindInfo" "pBinds"
  binds :: Vector SparseMemoryBind
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseBufferMemoryBindInfo' and
-- marshal a 'SparseBufferMemoryBindInfo' into it. The 'VkSparseBufferMemoryBindInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseBufferMemoryBindInfo :: SparseBufferMemoryBindInfo -> (VkSparseBufferMemoryBindInfo -> IO a) -> IO a
withCStructSparseBufferMemoryBindInfo marshalled cont = withVec withCStructSparseMemoryBind (binds (marshalled :: SparseBufferMemoryBindInfo)) (\pPBinds -> cont (VkSparseBufferMemoryBindInfo (buffer (marshalled :: SparseBufferMemoryBindInfo)) (fromIntegral (Data.Vector.length (binds (marshalled :: SparseBufferMemoryBindInfo)))) pPBinds))

-- | A function to read a 'VkSparseBufferMemoryBindInfo' and all additional
-- structures in the pointer chain into a 'SparseBufferMemoryBindInfo'.
fromCStructSparseBufferMemoryBindInfo :: VkSparseBufferMemoryBindInfo -> IO SparseBufferMemoryBindInfo
fromCStructSparseBufferMemoryBindInfo c = SparseBufferMemoryBindInfo <$> pure (vkBuffer (c :: VkSparseBufferMemoryBindInfo))
                                                                     -- Length valued member elided
                                                                     <*> (Data.Vector.generateM (fromIntegral (vkBindCount (c :: VkSparseBufferMemoryBindInfo))) (((fromCStructSparseMemoryBind <=<) . peekElemOff) (vkPBinds (c :: VkSparseBufferMemoryBindInfo))))

instance Zero SparseBufferMemoryBindInfo where
  zero = SparseBufferMemoryBindInfo zero
                                    Data.Vector.empty


-- | VkSparseImageFormatFlagBits - Bitmask specifying additional information
-- about a sparse image resource
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlags'
type SparseImageFormatFlagBits = VkSparseImageFormatFlagBits


{-# complete SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT, SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT, SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT :: SparseImageFormatFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT'
-- specifies that the image uses a single mip tail region for all array
-- layers.
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT :: (a ~ SparseImageFormatFlagBits) => a
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT'
-- specifies that the first mip level whose dimensions are not integer
-- multiples of the corresponding dimensions of the sparse image block
-- begins the mip tail region.
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT :: (a ~ SparseImageFormatFlagBits) => a
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT'
-- specifies that the image uses non-standard sparse image block
-- dimensions, and the @imageGranularity@ values do not match the standard
-- sparse image block dimensions for the given format.
pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT :: (a ~ SparseImageFormatFlagBits) => a
pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT

-- | VkSparseImageFormatFlags - Bitmask of VkSparseImageFormatFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlagBits',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
type SparseImageFormatFlags = SparseImageFormatFlagBits


-- | VkSparseImageFormatProperties - Structure specifying sparse image format
-- properties
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
data SparseImageFormatProperties = SparseImageFormatProperties
  { -- No documentation found for Nested "SparseImageFormatProperties" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "SparseImageFormatProperties" "imageGranularity"
  imageGranularity :: Extent3D
  , -- No documentation found for Nested "SparseImageFormatProperties" "flags"
  flags :: SparseImageFormatFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageFormatProperties' and
-- marshal a 'SparseImageFormatProperties' into it. The 'VkSparseImageFormatProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageFormatProperties :: SparseImageFormatProperties -> (VkSparseImageFormatProperties -> IO a) -> IO a
withCStructSparseImageFormatProperties marshalled cont = withCStructExtent3D (imageGranularity (marshalled :: SparseImageFormatProperties)) (\imageGranularity'' -> cont (VkSparseImageFormatProperties (aspectMask (marshalled :: SparseImageFormatProperties)) imageGranularity'' (flags (marshalled :: SparseImageFormatProperties))))

-- | A function to read a 'VkSparseImageFormatProperties' and all additional
-- structures in the pointer chain into a 'SparseImageFormatProperties'.
fromCStructSparseImageFormatProperties :: VkSparseImageFormatProperties -> IO SparseImageFormatProperties
fromCStructSparseImageFormatProperties c = SparseImageFormatProperties <$> pure (vkAspectMask (c :: VkSparseImageFormatProperties))
                                                                       <*> (fromCStructExtent3D (vkImageGranularity (c :: VkSparseImageFormatProperties)))
                                                                       <*> pure (vkFlags (c :: VkSparseImageFormatProperties))

instance Zero SparseImageFormatProperties where
  zero = SparseImageFormatProperties zero
                                     zero
                                     zero



-- | VkSparseImageMemoryBind - Structure specifying sparse image memory bind
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, and if any other resources are bound to
--     ranges of @memory@, the range of @memory@ being bound /must/ not
--     overlap with those bound ranges
--
-- -   @memory@ and @memoryOffset@ /must/ match the memory requirements of
--     the calling command’s @image@, as described in section
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-association>
--
-- -   @subresource@ /must/ be a valid image subresource for @image@ (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views>)
--
-- -   @offset.x@ /must/ be a multiple of the sparse image block width
--     ('Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'::@imageGranularity.width@)
--     of the image
--
-- -   @extent.width@ /must/ either be a multiple of the sparse image block
--     width of the image, or else (@extent.width@ + @offset.x@) /must/
--     equal the width of the image subresource
--
-- -   @offset.y@ /must/ be a multiple of the sparse image block height
--     ('Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'::@imageGranularity.height@)
--     of the image
--
-- -   @extent.height@ /must/ either be a multiple of the sparse image
--     block height of the image, or else (@extent.height@ + @offset.y@)
--     /must/ equal the height of the image subresource
--
-- -   @offset.z@ /must/ be a multiple of the sparse image block depth
--     ('Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'::@imageGranularity.depth@)
--     of the image
--
-- -   @extent.depth@ /must/ either be a multiple of the sparse image block
--     depth of the image, or else (@extent.depth@ + @offset.z@) /must/
--     equal the depth of the image subresource
--
-- == Valid Usage (Implicit)
--
-- -   @subresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource'
--     structure
--
-- -   If @memory@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @memory@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlags'
data SparseImageMemoryBind = SparseImageMemoryBind
  { -- No documentation found for Nested "SparseImageMemoryBind" "subresource"
  subresource :: ImageSubresource
  , -- No documentation found for Nested "SparseImageMemoryBind" "offset"
  offset :: Offset3D
  , -- No documentation found for Nested "SparseImageMemoryBind" "extent"
  extent :: Extent3D
  , -- No documentation found for Nested "SparseImageMemoryBind" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "SparseImageMemoryBind" "memoryOffset"
  memoryOffset :: DeviceSize
  , -- No documentation found for Nested "SparseImageMemoryBind" "flags"
  flags :: SparseMemoryBindFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageMemoryBind' and
-- marshal a 'SparseImageMemoryBind' into it. The 'VkSparseImageMemoryBind' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageMemoryBind :: SparseImageMemoryBind -> (VkSparseImageMemoryBind -> IO a) -> IO a
withCStructSparseImageMemoryBind marshalled cont = withCStructExtent3D (extent (marshalled :: SparseImageMemoryBind)) (\extent'' -> withCStructOffset3D (offset (marshalled :: SparseImageMemoryBind)) (\offset'' -> withCStructImageSubresource (subresource (marshalled :: SparseImageMemoryBind)) (\subresource'' -> cont (VkSparseImageMemoryBind subresource'' offset'' extent'' (memory (marshalled :: SparseImageMemoryBind)) (memoryOffset (marshalled :: SparseImageMemoryBind)) (flags (marshalled :: SparseImageMemoryBind))))))

-- | A function to read a 'VkSparseImageMemoryBind' and all additional
-- structures in the pointer chain into a 'SparseImageMemoryBind'.
fromCStructSparseImageMemoryBind :: VkSparseImageMemoryBind -> IO SparseImageMemoryBind
fromCStructSparseImageMemoryBind c = SparseImageMemoryBind <$> (fromCStructImageSubresource (vkSubresource (c :: VkSparseImageMemoryBind)))
                                                           <*> (fromCStructOffset3D (vkOffset (c :: VkSparseImageMemoryBind)))
                                                           <*> (fromCStructExtent3D (vkExtent (c :: VkSparseImageMemoryBind)))
                                                           <*> pure (vkMemory (c :: VkSparseImageMemoryBind))
                                                           <*> pure (vkMemoryOffset (c :: VkSparseImageMemoryBind))
                                                           <*> pure (vkFlags (c :: VkSparseImageMemoryBind))

instance Zero SparseImageMemoryBind where
  zero = SparseImageMemoryBind zero
                               zero
                               zero
                               zero
                               zero
                               zero



-- | VkSparseImageMemoryBindInfo - Structure specifying sparse image memory
-- bind info
--
-- == Valid Usage
--
-- -   The @subresource.mipLevel@ member of each element of @pBinds@ /must/
--     be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   The @subresource.arrayLayer@ member of each element of @pBinds@
--     /must/ be less than the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- == Valid Usage (Implicit)
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @pBinds@ /must/ be a valid pointer to an array of @bindCount@ valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind'
--     structures
--
-- -   @bindCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind'
data SparseImageMemoryBindInfo = SparseImageMemoryBindInfo
  { -- No documentation found for Nested "SparseImageMemoryBindInfo" "image"
  image :: Image
  -- Length valued member elided
  , -- No documentation found for Nested "SparseImageMemoryBindInfo" "pBinds"
  binds :: Vector SparseImageMemoryBind
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageMemoryBindInfo' and
-- marshal a 'SparseImageMemoryBindInfo' into it. The 'VkSparseImageMemoryBindInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageMemoryBindInfo :: SparseImageMemoryBindInfo -> (VkSparseImageMemoryBindInfo -> IO a) -> IO a
withCStructSparseImageMemoryBindInfo marshalled cont = withVec withCStructSparseImageMemoryBind (binds (marshalled :: SparseImageMemoryBindInfo)) (\pPBinds -> cont (VkSparseImageMemoryBindInfo (image (marshalled :: SparseImageMemoryBindInfo)) (fromIntegral (Data.Vector.length (binds (marshalled :: SparseImageMemoryBindInfo)))) pPBinds))

-- | A function to read a 'VkSparseImageMemoryBindInfo' and all additional
-- structures in the pointer chain into a 'SparseImageMemoryBindInfo'.
fromCStructSparseImageMemoryBindInfo :: VkSparseImageMemoryBindInfo -> IO SparseImageMemoryBindInfo
fromCStructSparseImageMemoryBindInfo c = SparseImageMemoryBindInfo <$> pure (vkImage (c :: VkSparseImageMemoryBindInfo))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkBindCount (c :: VkSparseImageMemoryBindInfo))) (((fromCStructSparseImageMemoryBind <=<) . peekElemOff) (vkPBinds (c :: VkSparseImageMemoryBindInfo))))

instance Zero SparseImageMemoryBindInfo where
  zero = SparseImageMemoryBindInfo zero
                                   Data.Vector.empty



-- | VkSparseImageMemoryRequirements - Structure specifying sparse image
-- memory requirements
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements'
data SparseImageMemoryRequirements = SparseImageMemoryRequirements
  { -- No documentation found for Nested "SparseImageMemoryRequirements" "formatProperties"
  formatProperties :: SparseImageFormatProperties
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailFirstLod"
  imageMipTailFirstLod :: Word32
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailSize"
  imageMipTailSize :: DeviceSize
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailOffset"
  imageMipTailOffset :: DeviceSize
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailStride"
  imageMipTailStride :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageMemoryRequirements' and
-- marshal a 'SparseImageMemoryRequirements' into it. The 'VkSparseImageMemoryRequirements' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageMemoryRequirements :: SparseImageMemoryRequirements -> (VkSparseImageMemoryRequirements -> IO a) -> IO a
withCStructSparseImageMemoryRequirements marshalled cont = withCStructSparseImageFormatProperties (formatProperties (marshalled :: SparseImageMemoryRequirements)) (\formatProperties'' -> cont (VkSparseImageMemoryRequirements formatProperties'' (imageMipTailFirstLod (marshalled :: SparseImageMemoryRequirements)) (imageMipTailSize (marshalled :: SparseImageMemoryRequirements)) (imageMipTailOffset (marshalled :: SparseImageMemoryRequirements)) (imageMipTailStride (marshalled :: SparseImageMemoryRequirements))))

-- | A function to read a 'VkSparseImageMemoryRequirements' and all additional
-- structures in the pointer chain into a 'SparseImageMemoryRequirements'.
fromCStructSparseImageMemoryRequirements :: VkSparseImageMemoryRequirements -> IO SparseImageMemoryRequirements
fromCStructSparseImageMemoryRequirements c = SparseImageMemoryRequirements <$> (fromCStructSparseImageFormatProperties (vkFormatProperties (c :: VkSparseImageMemoryRequirements)))
                                                                           <*> pure (vkImageMipTailFirstLod (c :: VkSparseImageMemoryRequirements))
                                                                           <*> pure (vkImageMipTailSize (c :: VkSparseImageMemoryRequirements))
                                                                           <*> pure (vkImageMipTailOffset (c :: VkSparseImageMemoryRequirements))
                                                                           <*> pure (vkImageMipTailStride (c :: VkSparseImageMemoryRequirements))

instance Zero SparseImageMemoryRequirements where
  zero = SparseImageMemoryRequirements zero
                                       zero
                                       zero
                                       zero
                                       zero



-- | VkSparseImageOpaqueMemoryBindInfo - Structure specifying sparse image
-- opaque memory bind info
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pBinds@ contains
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_MEMORY_BIND_METADATA_BIT',
--     the binding range defined /must/ be within the mip tail region of
--     the metadata aspect of @image@
--
-- == Valid Usage (Implicit)
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @pBinds@ /must/ be a valid pointer to an array of @bindCount@ valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind'
--     structures
--
-- -   @bindCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind'
data SparseImageOpaqueMemoryBindInfo = SparseImageOpaqueMemoryBindInfo
  { -- No documentation found for Nested "SparseImageOpaqueMemoryBindInfo" "image"
  image :: Image
  -- Length valued member elided
  , -- No documentation found for Nested "SparseImageOpaqueMemoryBindInfo" "pBinds"
  binds :: Vector SparseMemoryBind
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageOpaqueMemoryBindInfo' and
-- marshal a 'SparseImageOpaqueMemoryBindInfo' into it. The 'VkSparseImageOpaqueMemoryBindInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageOpaqueMemoryBindInfo :: SparseImageOpaqueMemoryBindInfo -> (VkSparseImageOpaqueMemoryBindInfo -> IO a) -> IO a
withCStructSparseImageOpaqueMemoryBindInfo marshalled cont = withVec withCStructSparseMemoryBind (binds (marshalled :: SparseImageOpaqueMemoryBindInfo)) (\pPBinds -> cont (VkSparseImageOpaqueMemoryBindInfo (image (marshalled :: SparseImageOpaqueMemoryBindInfo)) (fromIntegral (Data.Vector.length (binds (marshalled :: SparseImageOpaqueMemoryBindInfo)))) pPBinds))

-- | A function to read a 'VkSparseImageOpaqueMemoryBindInfo' and all additional
-- structures in the pointer chain into a 'SparseImageOpaqueMemoryBindInfo'.
fromCStructSparseImageOpaqueMemoryBindInfo :: VkSparseImageOpaqueMemoryBindInfo -> IO SparseImageOpaqueMemoryBindInfo
fromCStructSparseImageOpaqueMemoryBindInfo c = SparseImageOpaqueMemoryBindInfo <$> pure (vkImage (c :: VkSparseImageOpaqueMemoryBindInfo))
                                                                               -- Length valued member elided
                                                                               <*> (Data.Vector.generateM (fromIntegral (vkBindCount (c :: VkSparseImageOpaqueMemoryBindInfo))) (((fromCStructSparseMemoryBind <=<) . peekElemOff) (vkPBinds (c :: VkSparseImageOpaqueMemoryBindInfo))))

instance Zero SparseImageOpaqueMemoryBindInfo where
  zero = SparseImageOpaqueMemoryBindInfo zero
                                         Data.Vector.empty



-- | VkSparseMemoryBind - Structure specifying a sparse memory bind operation
--
-- = Description
--
-- The /binding range/ [@resourceOffset@, @resourceOffset@ + @size@) has
-- different constraints based on @flags@. If @flags@ contains
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_MEMORY_BIND_METADATA_BIT',
-- the binding range /must/ be within the mip tail region of the metadata
-- aspect. This metadata region is defined by:
--
-- -   metadataRegion = [base, base + @imageMipTailSize@)
--
-- -   base = @imageMipTailOffset@ + @imageMipTailStride@ × n
--
-- and @imageMipTailOffset@, @imageMipTailSize@, and @imageMipTailStride@
-- values are from the
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
-- corresponding to the metadata aspect of the image, and n is a valid
-- array layer index for the image,
--
-- @imageMipTailStride@ is considered to be zero for aspects where
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'::@formatProperties.flags@
-- contains
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT'.
--
-- If @flags@ does not contain
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_MEMORY_BIND_METADATA_BIT',
-- the binding range /must/ be within the range
-- [0,'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@).
--
-- == Valid Usage
--
-- -   If @memory@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @memory@ and
--     @memoryOffset@ /must/ match the memory requirements of the resource,
--     as described in section
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-association>
--
-- -   If @memory@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @memory@ /must/
--     not have been created with a memory type that reports
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
--     bit set
--
-- -   @size@ /must/ be greater than @0@
--
-- -   @resourceOffset@ /must/ be less than the size of the resource
--
-- -   @size@ /must/ be less than or equal to the size of the resource
--     minus @resourceOffset@
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @size@ /must/ be less than or equal to the size of @memory@ minus
--     @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   If @memory@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @memory@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseBufferMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageOpaqueMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlags'
data SparseMemoryBind = SparseMemoryBind
  { -- No documentation found for Nested "SparseMemoryBind" "resourceOffset"
  resourceOffset :: DeviceSize
  , -- No documentation found for Nested "SparseMemoryBind" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "SparseMemoryBind" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "SparseMemoryBind" "memoryOffset"
  memoryOffset :: DeviceSize
  , -- No documentation found for Nested "SparseMemoryBind" "flags"
  flags :: SparseMemoryBindFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseMemoryBind' and
-- marshal a 'SparseMemoryBind' into it. The 'VkSparseMemoryBind' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseMemoryBind :: SparseMemoryBind -> (VkSparseMemoryBind -> IO a) -> IO a
withCStructSparseMemoryBind marshalled cont = cont (VkSparseMemoryBind (resourceOffset (marshalled :: SparseMemoryBind)) (size (marshalled :: SparseMemoryBind)) (memory (marshalled :: SparseMemoryBind)) (memoryOffset (marshalled :: SparseMemoryBind)) (flags (marshalled :: SparseMemoryBind)))

-- | A function to read a 'VkSparseMemoryBind' and all additional
-- structures in the pointer chain into a 'SparseMemoryBind'.
fromCStructSparseMemoryBind :: VkSparseMemoryBind -> IO SparseMemoryBind
fromCStructSparseMemoryBind c = SparseMemoryBind <$> pure (vkResourceOffset (c :: VkSparseMemoryBind))
                                                 <*> pure (vkSize (c :: VkSparseMemoryBind))
                                                 <*> pure (vkMemory (c :: VkSparseMemoryBind))
                                                 <*> pure (vkMemoryOffset (c :: VkSparseMemoryBind))
                                                 <*> pure (vkFlags (c :: VkSparseMemoryBind))

instance Zero SparseMemoryBind where
  zero = SparseMemoryBind zero
                          zero
                          zero
                          zero
                          zero


-- | VkSparseMemoryBindFlagBits - Bitmask specifying usage of a sparse memory
-- binding operation
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlags'
type SparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits


{-# complete SPARSE_MEMORY_BIND_METADATA_BIT :: SparseMemoryBindFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_MEMORY_BIND_METADATA_BIT'
-- specifies that the memory being bound is only for the metadata aspect.
pattern SPARSE_MEMORY_BIND_METADATA_BIT :: (a ~ SparseMemoryBindFlagBits) => a
pattern SPARSE_MEMORY_BIND_METADATA_BIT = VK_SPARSE_MEMORY_BIND_METADATA_BIT

-- | VkSparseMemoryBindFlags - Bitmask of VkSparseMemoryBindFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlagBits'
type SparseMemoryBindFlags = SparseMemoryBindFlagBits


-- | vkGetImageSparseMemoryRequirements - Query the memory requirements for a
-- sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
--     object to get the memory requirements for.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
--     structures.
--
-- = Description
--
-- If @pSparseMemoryRequirements@ is @NULL@, then the number of sparse
-- memory requirements available is returned in
-- @pSparseMemoryRequirementCount@. Otherwise,
-- @pSparseMemoryRequirementCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSparseMemoryRequirements@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pSparseMemoryRequirements@. If
-- @pSparseMemoryRequirementCount@ is less than the number of sparse memory
-- requirements available, at most @pSparseMemoryRequirementCount@
-- structures will be written.
--
-- If the image was not created with
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- then @pSparseMemoryRequirementCount@ will be set to zero and
-- @pSparseMemoryRequirements@ will not be written to.
--
-- __Note__
--
-- It is legal for an implementation to report a larger value in
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
-- than would be obtained by adding together memory sizes for all
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
-- returned by
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements'.
-- This /may/ occur when the implementation requires unused padding in the
-- address range describing the resource.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
--     structures
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
getNumImageSparseMemoryRequirements :: Device ->  Image ->  IO (Word32)
getNumImageSparseMemoryRequirements = \(Device device' commandTable) -> \image' -> alloca (\pSparseMemoryRequirementCount' -> vkGetImageSparseMemoryRequirements commandTable device' image' pSparseMemoryRequirementCount' nullPtr *> (peek pSparseMemoryRequirementCount'))

-- | vkGetImageSparseMemoryRequirements - Query the memory requirements for a
-- sparse image
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
--     object to get the memory requirements for.
--
-- -   @pSparseMemoryRequirementCount@ is a pointer to an integer related
--     to the number of sparse memory requirements available or queried, as
--     described below.
--
-- -   @pSparseMemoryRequirements@ is either @NULL@ or a pointer to an
--     array of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
--     structures.
--
-- = Description
--
-- If @pSparseMemoryRequirements@ is @NULL@, then the number of sparse
-- memory requirements available is returned in
-- @pSparseMemoryRequirementCount@. Otherwise,
-- @pSparseMemoryRequirementCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSparseMemoryRequirements@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pSparseMemoryRequirements@. If
-- @pSparseMemoryRequirementCount@ is less than the number of sparse memory
-- requirements available, at most @pSparseMemoryRequirementCount@
-- structures will be written.
--
-- If the image was not created with
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- then @pSparseMemoryRequirementCount@ will be set to zero and
-- @pSparseMemoryRequirements@ will not be written to.
--
-- __Note__
--
-- It is legal for an implementation to report a larger value in
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
-- than would be obtained by adding together memory sizes for all
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
-- returned by
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements'.
-- This /may/ occur when the implementation requires unused padding in the
-- address range describing the resource.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @pSparseMemoryRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pSparseMemoryRequirementCount@ is not
--     @0@, and @pSparseMemoryRequirements@ is not @NULL@,
--     @pSparseMemoryRequirements@ /must/ be a valid pointer to an array of
--     @pSparseMemoryRequirementCount@
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
--     structures
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements'
getImageSparseMemoryRequirements :: Device ->  Image ->  Word32 ->  IO (Vector SparseImageMemoryRequirements)
getImageSparseMemoryRequirements = \(Device device' commandTable) -> \image' -> \sparseMemoryRequirementCount' -> allocaArray (fromIntegral sparseMemoryRequirementCount') (\pSparseMemoryRequirements' -> with sparseMemoryRequirementCount' (\pSparseMemoryRequirementCount' -> vkGetImageSparseMemoryRequirements commandTable device' image' pSparseMemoryRequirementCount' pSparseMemoryRequirements' *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageMemoryRequirements <=< peekElemOff p) pSparseMemoryRequirements') =<< (fromIntegral <$> (peek pSparseMemoryRequirementCount'))))))
-- | Returns all the values available from 'getImageSparseMemoryRequirements'.
getAllImageSparseMemoryRequirements :: Device ->  Image ->  IO (Vector SparseImageMemoryRequirements)
getAllImageSparseMemoryRequirements device' image' =
  getNumImageSparseMemoryRequirements device' image'
    >>= \num -> getImageSparseMemoryRequirements device' image' num



-- | vkGetPhysicalDeviceSparseImageFormatProperties - Retrieve properties of
-- an image format applied to sparse images
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     sparse image capabilities.
--
-- -   @format@ is the image format.
--
-- -   @type@ is the dimensionality of image.
--
-- -   @samples@ is the number of samples per texel as defined in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'.
--
-- -   @usage@ is a bitmask describing the intended usage of the image.
--
-- -   @tiling@ is the tiling arrangement of the texel blocks in memory.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     sparse format properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of sparse format properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of sparse format properties
-- available, at most @pPropertyCount@ structures will be written.
--
-- If
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- is not supported for the given arguments, @pPropertyCount@ will be set
-- to zero upon return, and no data will be written to @pProperties@.
--
-- Multiple aspects are returned for depth\/stencil images that are
-- implemented as separate planes by the implementation. The depth and
-- stencil data planes each have unique
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
-- data.
--
-- Depth\/stencil images with depth and stencil data interleaved into a
-- single plane will return a single
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
-- structure with the @aspectMask@ set to
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- |
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'.
--
-- == Valid Usage
--
-- -   @samples@ /must/ be a bit value that is set in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@sampleCounts@
--     returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
--     with @format@, @type@, @tiling@, and @usage@ equal to those in this
--     command and @flags@ equal to the value that is set in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@ when the
--     image is created
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @type@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @tiling@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
getNumPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  IO (Word32)
getNumPhysicalDeviceSparseImageFormatProperties = \(PhysicalDevice physicalDevice' commandTable) -> \format' -> \type' -> \samples' -> \usage' -> \tiling' -> alloca (\pPropertyCount' -> vkGetPhysicalDeviceSparseImageFormatProperties commandTable physicalDevice' format' type' samples' usage' tiling' pPropertyCount' nullPtr *> (peek pPropertyCount'))

-- | vkGetPhysicalDeviceSparseImageFormatProperties - Retrieve properties of
-- an image format applied to sparse images
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     sparse image capabilities.
--
-- -   @format@ is the image format.
--
-- -   @type@ is the dimensionality of image.
--
-- -   @samples@ is the number of samples per texel as defined in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'.
--
-- -   @usage@ is a bitmask describing the intended usage of the image.
--
-- -   @tiling@ is the tiling arrangement of the texel blocks in memory.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     sparse format properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of sparse format properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of sparse format properties
-- available, at most @pPropertyCount@ structures will be written.
--
-- If
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- is not supported for the given arguments, @pPropertyCount@ will be set
-- to zero upon return, and no data will be written to @pProperties@.
--
-- Multiple aspects are returned for depth\/stencil images that are
-- implemented as separate planes by the implementation. The depth and
-- stencil data planes each have unique
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
-- data.
--
-- Depth\/stencil images with depth and stencil data interleaved into a
-- single plane will return a single
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
-- structure with the @aspectMask@ set to
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
-- |
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'.
--
-- == Valid Usage
--
-- -   @samples@ /must/ be a bit value that is set in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'::@sampleCounts@
--     returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
--     with @format@, @type@, @tiling@, and @usage@ equal to those in this
--     command and @flags@ equal to the value that is set in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@ when the
--     image is created
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Core.VkFormat'
--     value
--
-- -   @type@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @tiling@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
getPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  Word32 ->  IO (Vector SparseImageFormatProperties)
getPhysicalDeviceSparseImageFormatProperties = \(PhysicalDevice physicalDevice' commandTable) -> \format' -> \type' -> \samples' -> \usage' -> \tiling' -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetPhysicalDeviceSparseImageFormatProperties commandTable physicalDevice' format' type' samples' usage' tiling' pPropertyCount' pProperties' *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageFormatProperties <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount'))))))
-- | Returns all the values available from 'getPhysicalDeviceSparseImageFormatProperties'.
getAllPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  IO (Vector SparseImageFormatProperties)
getAllPhysicalDeviceSparseImageFormatProperties physicalDevice' format' type' samples' usage' tiling' =
  getNumPhysicalDeviceSparseImageFormatProperties physicalDevice' format' type' samples' usage' tiling'
    >>= \num -> getPhysicalDeviceSparseImageFormatProperties physicalDevice' format' type' samples' usage' tiling' num



-- | vkQueueBindSparse - Bind device memory to a sparse resource object
--
-- = Parameters
--
-- -   @queue@ is the queue that the sparse binding operations will be
--     submitted to.
--
-- -   @bindInfoCount@ is the number of elements in the @pBindInfo@ array.
--
-- -   @pBindInfo@ is an array of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo'
--     structures, each specifying a sparse binding submission batch.
--
-- -   @fence@ is an /optional/ handle to a fence to be signaled. If
--     @fence@ is not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     it defines a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse'
-- is a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pBindInfo@ as an instance of
-- the
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo'
-- structure. Batches begin execution in the order they appear in
-- @pBindInfo@, but /may/ complete out of order.
--
-- Within a batch, a given range of a resource /must/ not be bound more
-- than once. Across batches, if a range is to be bound to one allocation
-- and offset and then to another allocation and offset, then the
-- application /must/ guarantee (usually using semaphores) that the binding
-- operations are executed in the correct order, as well as to order
-- binding operations against the execution of command buffer submissions.
--
-- As no operation to
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse'
-- causes any pipeline stage to access memory, synchronization primitives
-- used in this command effectively only define execution dependencies.
--
-- Additional information about fence and semaphore operation is described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- == Valid Usage
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     be unsignaled
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     not be associated with any other queue command that has not yet
--     completed execution on that queue
--
-- -   Each element of the @pSignalSemaphores@ member of each element of
--     @pBindInfo@ /must/ be unsignaled when the semaphore signal operation
--     it defines is executed on the device
--
-- -   When a semaphore unsignal operation defined by any element of the
--     @pWaitSemaphores@ member of any element of @pBindInfo@ executes on
--     @queue@, no other queue /must/ be waiting on the same semaphore.
--
-- -   All elements of the @pWaitSemaphores@ member of all elements of
--     @pBindInfo@ /must/ be semaphores that are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution.
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
--     handle
--
-- -   If @bindInfoCount@ is not @0@, @pBindInfo@ /must/ be a valid pointer
--     to an array of @bindInfoCount@ valid
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo'
--     structures
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Queue.VkFence' handle
--
-- -   The @queue@ /must/ support sparse binding operations
--
-- -   Both of @fence@, and @queue@ that are valid handles /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @pBindInfo@[].pWaitSemaphores[] /must/ be externally
--     synchronized
--
-- -   Host access to @pBindInfo@[].pSignalSemaphores[] /must/ be
--     externally synchronized
--
-- -   Host access to @pBindInfo@[].pBufferBinds[].buffer /must/ be
--     externally synchronized
--
-- -   Host access to @pBindInfo@[].pImageOpaqueBinds[].image /must/ be
--     externally synchronized
--
-- -   Host access to @pBindInfo@[].pImageBinds[].image /must/ be
--     externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | -               | -               | SPARSE_BINDING  | -               |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
queueBindSparse :: Queue ->  Vector BindSparseInfo ->  Fence ->  IO ()
queueBindSparse = \(Queue queue' commandTable) -> \bindInfo' -> \fence' -> withVec withCStructBindSparseInfo bindInfo' (\pBindInfo' -> vkQueueBindSparse commandTable queue' (fromIntegral $ Data.Vector.length bindInfo') pBindInfo' fence' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))
