{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  BindSparseInfo(..)
  , 
#endif
  ImageAspectFlagBits
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
  , ImageSubresource(..)
  , Offset3D(..)
  , SparseBufferMemoryBindInfo(..)
  , SparseImageFormatFlagBits
  , pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
  , pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
  , pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
  , SparseImageFormatFlags
  , SparseImageFormatProperties(..)
  , SparseImageMemoryBind(..)
  , SparseImageMemoryBindInfo(..)
  , SparseImageMemoryRequirements(..)
  , SparseImageOpaqueMemoryBindInfo(..)
  , SparseMemoryBind(..)
  , SparseMemoryBindFlagBits
  , pattern SPARSE_MEMORY_BIND_METADATA_BIT
  , SparseMemoryBindFlags
#if defined(VK_USE_PLATFORM_GGP)
  , getNumImageSparseMemoryRequirements
  , getImageSparseMemoryRequirements
  , getAllImageSparseMemoryRequirements
  , getNumPhysicalDeviceSparseImageFormatProperties
  , getPhysicalDeviceSparseImageFormatProperties
  , getAllPhysicalDeviceSparseImageFormatProperties
#endif
  , queueBindSparse
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif
import Data.Word
  ( Word32
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits(..)
  , VkSparseImageFormatFlagBits(..)
  , VkSparseMemoryBindFlagBits(..)
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( vkGetImageSparseMemoryRequirements
  , vkGetPhysicalDeviceSparseImageFormatProperties
  )
#endif
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Extent3D(..)
  , DeviceSize
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , PhysicalDevice(..)
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , SampleCountFlagBits
  )
#endif
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
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )
#endif
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindSparseInfo"
data BindSparseInfo = BindSparseInfo
  { -- No documentation found for Nested "BindSparseInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindSparseInfo" "pWaitSemaphores"
  waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "BindSparseInfo" "pBufferBinds"
  bufferBinds :: Vector SparseBufferMemoryBindInfo
  , -- No documentation found for Nested "BindSparseInfo" "pImageOpaqueBinds"
  imageOpaqueBinds :: Vector SparseImageOpaqueMemoryBindInfo
  , -- No documentation found for Nested "BindSparseInfo" "pImageBinds"
  imageBinds :: Vector SparseImageMemoryBindInfo
  , -- No documentation found for Nested "BindSparseInfo" "pSignalSemaphores"
  signalSemaphores :: Vector Semaphore
  }
  deriving (Show, Eq)

instance Zero BindSparseInfo where
  zero = BindSparseInfo Nothing
                        mempty
                        mempty
                        mempty
                        mempty
                        mempty

#endif

-- No documentation found for TopLevel "ImageAspectFlagBits"
type ImageAspectFlagBits = VkImageAspectFlagBits


{-# complete IMAGE_ASPECT_COLOR_BIT, IMAGE_ASPECT_DEPTH_BIT, IMAGE_ASPECT_STENCIL_BIT, IMAGE_ASPECT_METADATA_BIT, IMAGE_ASPECT_PLANE_0_BIT, IMAGE_ASPECT_PLANE_1_BIT, IMAGE_ASPECT_PLANE_2_BIT, IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT, IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT, IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT, IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT :: ImageAspectFlagBits #-}


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_COLOR_BIT"
pattern IMAGE_ASPECT_COLOR_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_COLOR_BIT = VK_IMAGE_ASPECT_COLOR_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_DEPTH_BIT"
pattern IMAGE_ASPECT_DEPTH_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_DEPTH_BIT = VK_IMAGE_ASPECT_DEPTH_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_STENCIL_BIT"
pattern IMAGE_ASPECT_STENCIL_BIT :: (a ~ ImageAspectFlagBits) => a
pattern IMAGE_ASPECT_STENCIL_BIT = VK_IMAGE_ASPECT_STENCIL_BIT


-- No documentation found for Nested "ImageAspectFlagBits" "IMAGE_ASPECT_METADATA_BIT"
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

-- No documentation found for TopLevel "ImageAspectFlags"
type ImageAspectFlags = ImageAspectFlagBits


-- No documentation found for TopLevel "VkImageSubresource"
data ImageSubresource = ImageSubresource
  { -- No documentation found for Nested "ImageSubresource" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresource" "mipLevel"
  mipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresource" "arrayLayer"
  arrayLayer :: Word32
  }
  deriving (Show, Eq)

instance Zero ImageSubresource where
  zero = ImageSubresource zero
                          zero
                          zero



-- No documentation found for TopLevel "VkOffset3D"
data Offset3D = Offset3D
  { -- No documentation found for Nested "Offset3D" "x"
  x :: Int32
  , -- No documentation found for Nested "Offset3D" "y"
  y :: Int32
  , -- No documentation found for Nested "Offset3D" "z"
  z :: Int32
  }
  deriving (Show, Eq)

instance Zero Offset3D where
  zero = Offset3D zero
                  zero
                  zero



-- No documentation found for TopLevel "VkSparseBufferMemoryBindInfo"
data SparseBufferMemoryBindInfo = SparseBufferMemoryBindInfo
  { -- No documentation found for Nested "SparseBufferMemoryBindInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "SparseBufferMemoryBindInfo" "pBinds"
  binds :: Vector SparseMemoryBind
  }
  deriving (Show, Eq)

instance Zero SparseBufferMemoryBindInfo where
  zero = SparseBufferMemoryBindInfo zero
                                    mempty


-- No documentation found for TopLevel "SparseImageFormatFlagBits"
type SparseImageFormatFlagBits = VkSparseImageFormatFlagBits


{-# complete SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT, SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT, SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT :: SparseImageFormatFlagBits #-}


-- No documentation found for Nested "SparseImageFormatFlagBits" "SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT :: (a ~ SparseImageFormatFlagBits) => a
pattern SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT


-- No documentation found for Nested "SparseImageFormatFlagBits" "SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT :: (a ~ SparseImageFormatFlagBits) => a
pattern SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT


-- No documentation found for Nested "SparseImageFormatFlagBits" "SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT :: (a ~ SparseImageFormatFlagBits) => a
pattern SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT

-- No documentation found for TopLevel "SparseImageFormatFlags"
type SparseImageFormatFlags = SparseImageFormatFlagBits


-- No documentation found for TopLevel "VkSparseImageFormatProperties"
data SparseImageFormatProperties = SparseImageFormatProperties
  { -- No documentation found for Nested "SparseImageFormatProperties" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "SparseImageFormatProperties" "imageGranularity"
  imageGranularity :: Extent3D
  , -- No documentation found for Nested "SparseImageFormatProperties" "flags"
  flags :: SparseImageFormatFlags
  }
  deriving (Show, Eq)

instance Zero SparseImageFormatProperties where
  zero = SparseImageFormatProperties zero
                                     zero
                                     zero



-- No documentation found for TopLevel "VkSparseImageMemoryBind"
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

instance Zero SparseImageMemoryBind where
  zero = SparseImageMemoryBind zero
                               zero
                               zero
                               zero
                               zero
                               zero



-- No documentation found for TopLevel "VkSparseImageMemoryBindInfo"
data SparseImageMemoryBindInfo = SparseImageMemoryBindInfo
  { -- No documentation found for Nested "SparseImageMemoryBindInfo" "image"
  image :: Image
  , -- No documentation found for Nested "SparseImageMemoryBindInfo" "pBinds"
  binds :: Vector SparseImageMemoryBind
  }
  deriving (Show, Eq)

instance Zero SparseImageMemoryBindInfo where
  zero = SparseImageMemoryBindInfo zero
                                   mempty



-- No documentation found for TopLevel "VkSparseImageMemoryRequirements"
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

instance Zero SparseImageMemoryRequirements where
  zero = SparseImageMemoryRequirements zero
                                       zero
                                       zero
                                       zero
                                       zero



-- No documentation found for TopLevel "VkSparseImageOpaqueMemoryBindInfo"
data SparseImageOpaqueMemoryBindInfo = SparseImageOpaqueMemoryBindInfo
  { -- No documentation found for Nested "SparseImageOpaqueMemoryBindInfo" "image"
  image :: Image
  , -- No documentation found for Nested "SparseImageOpaqueMemoryBindInfo" "pBinds"
  binds :: Vector SparseMemoryBind
  }
  deriving (Show, Eq)

instance Zero SparseImageOpaqueMemoryBindInfo where
  zero = SparseImageOpaqueMemoryBindInfo zero
                                         mempty



-- No documentation found for TopLevel "VkSparseMemoryBind"
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

instance Zero SparseMemoryBind where
  zero = SparseMemoryBind zero
                          zero
                          zero
                          zero
                          zero


-- No documentation found for TopLevel "SparseMemoryBindFlagBits"
type SparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits


{-# complete SPARSE_MEMORY_BIND_METADATA_BIT :: SparseMemoryBindFlagBits #-}


-- No documentation found for Nested "SparseMemoryBindFlagBits" "SPARSE_MEMORY_BIND_METADATA_BIT"
pattern SPARSE_MEMORY_BIND_METADATA_BIT :: (a ~ SparseMemoryBindFlagBits) => a
pattern SPARSE_MEMORY_BIND_METADATA_BIT = VK_SPARSE_MEMORY_BIND_METADATA_BIT

-- No documentation found for TopLevel "SparseMemoryBindFlags"
type SparseMemoryBindFlags = SparseMemoryBindFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements"
getNumImageSparseMemoryRequirements :: Device ->  Image ->  IO (Word32)
getNumImageSparseMemoryRequirements = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements"
getImageSparseMemoryRequirements :: Device ->  Image ->  Word32 ->  IO (Vector SparseImageMemoryRequirements)
getImageSparseMemoryRequirements = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getImageSparseMemoryRequirements'.
getAllImageSparseMemoryRequirements :: Device ->  Image ->  IO (Vector SparseImageMemoryRequirements)
getAllImageSparseMemoryRequirements device' image' =
  getNumImageSparseMemoryRequirements device' image'
    >>= \num -> getImageSparseMemoryRequirements device' image' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties"
getNumPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  IO (Word32)
getNumPhysicalDeviceSparseImageFormatProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties"
getPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  Word32 ->  IO (Vector SparseImageFormatProperties)
getPhysicalDeviceSparseImageFormatProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceSparseImageFormatProperties'.
getAllPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  IO (Vector SparseImageFormatProperties)
getAllPhysicalDeviceSparseImageFormatProperties physicalDevice' format' type' samples' usage' tiling' =
  getNumPhysicalDeviceSparseImageFormatProperties physicalDevice' format' type' samples' usage' tiling'
    >>= \num -> getPhysicalDeviceSparseImageFormatProperties physicalDevice' format' type' samples' usage' tiling' num

#endif


-- No documentation found for TopLevel "vkQueueBindSparse"
queueBindSparse :: Queue ->  Vector BindSparseInfo ->  Fence ->  IO ()
queueBindSparse = undefined {- {wrapped (pretty cName) :: Doc ()} -}
