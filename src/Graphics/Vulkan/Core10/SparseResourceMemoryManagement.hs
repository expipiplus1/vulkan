{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( withCStructBindSparseInfo
  , fromCStructBindSparseInfo
  , BindSparseInfo(..)
  , ImageAspectFlagBits
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
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( getImageSparseMemoryRequirements
  , getPhysicalDeviceSparseImageFormatProperties
  , queueBindSparse
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
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


-- No documentation found for TopLevel "BindSparseInfo"
data BindSparseInfo = BindSparseInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BindSparseInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pWaitSemaphores"
  vkPWaitSemaphores :: Vector Semaphore
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pBufferBinds"
  vkPBufferBinds :: Vector SparseBufferMemoryBindInfo
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pImageOpaqueBinds"
  vkPImageOpaqueBinds :: Vector SparseImageOpaqueMemoryBindInfo
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pImageBinds"
  vkPImageBinds :: Vector SparseImageMemoryBindInfo
  -- Length valued member elided
  , -- No documentation found for Nested "BindSparseInfo" "pSignalSemaphores"
  vkPSignalSemaphores :: Vector Semaphore
  }
  deriving (Show, Eq)
withCStructBindSparseInfo :: BindSparseInfo -> (VkBindSparseInfo -> IO a) -> IO a
withCStructBindSparseInfo from cont = withVec (&) (vkPSignalSemaphores (from :: BindSparseInfo)) (\pSignalSemaphores -> withVec withCStructSparseImageMemoryBindInfo (vkPImageBinds (from :: BindSparseInfo)) (\pImageBinds -> withVec withCStructSparseImageOpaqueMemoryBindInfo (vkPImageOpaqueBinds (from :: BindSparseInfo)) (\pImageOpaqueBinds -> withVec withCStructSparseBufferMemoryBindInfo (vkPBufferBinds (from :: BindSparseInfo)) (\pBufferBinds -> withVec (&) (vkPWaitSemaphores (from :: BindSparseInfo)) (\pWaitSemaphores -> maybeWith withSomeVkStruct (vkPNext (from :: BindSparseInfo)) (\pPNext -> cont (VkBindSparseInfo VK_STRUCTURE_TYPE_BIND_SPARSE_INFO pPNext (fromIntegral (Data.Vector.length (vkPWaitSemaphores (from :: BindSparseInfo)))) pWaitSemaphores (fromIntegral (Data.Vector.length (vkPBufferBinds (from :: BindSparseInfo)))) pBufferBinds (fromIntegral (Data.Vector.length (vkPImageOpaqueBinds (from :: BindSparseInfo)))) pImageOpaqueBinds (fromIntegral (Data.Vector.length (vkPImageBinds (from :: BindSparseInfo)))) pImageBinds (fromIntegral (Data.Vector.length (vkPSignalSemaphores (from :: BindSparseInfo)))) pSignalSemaphores)))))))
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
-- No documentation found for TopLevel "ImageAspectFlagBits"
type ImageAspectFlagBits = VkImageAspectFlagBits
-- No documentation found for TopLevel "ImageAspectFlags"
type ImageAspectFlags = ImageAspectFlagBits
-- No documentation found for TopLevel "ImageSubresource"
data ImageSubresource = ImageSubresource
  { -- No documentation found for Nested "ImageSubresource" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresource" "mipLevel"
  vkMipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresource" "arrayLayer"
  vkArrayLayer :: Word32
  }
  deriving (Show, Eq)
withCStructImageSubresource :: ImageSubresource -> (VkImageSubresource -> IO a) -> IO a
withCStructImageSubresource from cont = cont (VkImageSubresource (vkAspectMask (from :: ImageSubresource)) (vkMipLevel (from :: ImageSubresource)) (vkArrayLayer (from :: ImageSubresource)))
fromCStructImageSubresource :: VkImageSubresource -> IO ImageSubresource
fromCStructImageSubresource c = ImageSubresource <$> pure (vkAspectMask (c :: VkImageSubresource))
                                                 <*> pure (vkMipLevel (c :: VkImageSubresource))
                                                 <*> pure (vkArrayLayer (c :: VkImageSubresource))
-- No documentation found for TopLevel "Offset3D"
data Offset3D = Offset3D
  { -- No documentation found for Nested "Offset3D" "x"
  vkX :: Int32
  , -- No documentation found for Nested "Offset3D" "y"
  vkY :: Int32
  , -- No documentation found for Nested "Offset3D" "z"
  vkZ :: Int32
  }
  deriving (Show, Eq)
withCStructOffset3D :: Offset3D -> (VkOffset3D -> IO a) -> IO a
withCStructOffset3D from cont = cont (VkOffset3D (vkX (from :: Offset3D)) (vkY (from :: Offset3D)) (vkZ (from :: Offset3D)))
fromCStructOffset3D :: VkOffset3D -> IO Offset3D
fromCStructOffset3D c = Offset3D <$> pure (vkX (c :: VkOffset3D))
                                 <*> pure (vkY (c :: VkOffset3D))
                                 <*> pure (vkZ (c :: VkOffset3D))
-- No documentation found for TopLevel "SparseBufferMemoryBindInfo"
data SparseBufferMemoryBindInfo = SparseBufferMemoryBindInfo
  { -- No documentation found for Nested "SparseBufferMemoryBindInfo" "buffer"
  vkBuffer :: Buffer
  -- Length valued member elided
  , -- No documentation found for Nested "SparseBufferMemoryBindInfo" "pBinds"
  vkPBinds :: Vector SparseMemoryBind
  }
  deriving (Show, Eq)
withCStructSparseBufferMemoryBindInfo :: SparseBufferMemoryBindInfo -> (VkSparseBufferMemoryBindInfo -> IO a) -> IO a
withCStructSparseBufferMemoryBindInfo from cont = withVec withCStructSparseMemoryBind (vkPBinds (from :: SparseBufferMemoryBindInfo)) (\pBinds -> cont (VkSparseBufferMemoryBindInfo (vkBuffer (from :: SparseBufferMemoryBindInfo)) (fromIntegral (Data.Vector.length (vkPBinds (from :: SparseBufferMemoryBindInfo)))) pBinds))
fromCStructSparseBufferMemoryBindInfo :: VkSparseBufferMemoryBindInfo -> IO SparseBufferMemoryBindInfo
fromCStructSparseBufferMemoryBindInfo c = SparseBufferMemoryBindInfo <$> pure (vkBuffer (c :: VkSparseBufferMemoryBindInfo))
                                                                     -- Length valued member elided
                                                                     <*> (Data.Vector.generateM (fromIntegral (vkBindCount (c :: VkSparseBufferMemoryBindInfo))) (((fromCStructSparseMemoryBind <=<) . peekElemOff) (vkPBinds (c :: VkSparseBufferMemoryBindInfo))))
-- No documentation found for TopLevel "SparseImageFormatFlagBits"
type SparseImageFormatFlagBits = VkSparseImageFormatFlagBits
-- No documentation found for TopLevel "SparseImageFormatFlags"
type SparseImageFormatFlags = SparseImageFormatFlagBits
-- No documentation found for TopLevel "SparseImageFormatProperties"
data SparseImageFormatProperties = SparseImageFormatProperties
  { -- No documentation found for Nested "SparseImageFormatProperties" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "SparseImageFormatProperties" "imageGranularity"
  vkImageGranularity :: Extent3D
  , -- No documentation found for Nested "SparseImageFormatProperties" "flags"
  vkFlags :: SparseImageFormatFlags
  }
  deriving (Show, Eq)
withCStructSparseImageFormatProperties :: SparseImageFormatProperties -> (VkSparseImageFormatProperties -> IO a) -> IO a
withCStructSparseImageFormatProperties from cont = withCStructExtent3D (vkImageGranularity (from :: SparseImageFormatProperties)) (\imageGranularity -> cont (VkSparseImageFormatProperties (vkAspectMask (from :: SparseImageFormatProperties)) imageGranularity (vkFlags (from :: SparseImageFormatProperties))))
fromCStructSparseImageFormatProperties :: VkSparseImageFormatProperties -> IO SparseImageFormatProperties
fromCStructSparseImageFormatProperties c = SparseImageFormatProperties <$> pure (vkAspectMask (c :: VkSparseImageFormatProperties))
                                                                       <*> (fromCStructExtent3D (vkImageGranularity (c :: VkSparseImageFormatProperties)))
                                                                       <*> pure (vkFlags (c :: VkSparseImageFormatProperties))
-- No documentation found for TopLevel "SparseImageMemoryBind"
data SparseImageMemoryBind = SparseImageMemoryBind
  { -- No documentation found for Nested "SparseImageMemoryBind" "subresource"
  vkSubresource :: ImageSubresource
  , -- No documentation found for Nested "SparseImageMemoryBind" "offset"
  vkOffset :: Offset3D
  , -- No documentation found for Nested "SparseImageMemoryBind" "extent"
  vkExtent :: Extent3D
  , -- No documentation found for Nested "SparseImageMemoryBind" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "SparseImageMemoryBind" "memoryOffset"
  vkMemoryOffset :: DeviceSize
  , -- No documentation found for Nested "SparseImageMemoryBind" "flags"
  vkFlags :: SparseMemoryBindFlags
  }
  deriving (Show, Eq)
withCStructSparseImageMemoryBind :: SparseImageMemoryBind -> (VkSparseImageMemoryBind -> IO a) -> IO a
withCStructSparseImageMemoryBind from cont = withCStructExtent3D (vkExtent (from :: SparseImageMemoryBind)) (\extent -> withCStructOffset3D (vkOffset (from :: SparseImageMemoryBind)) (\offset -> withCStructImageSubresource (vkSubresource (from :: SparseImageMemoryBind)) (\subresource -> cont (VkSparseImageMemoryBind subresource offset extent (vkMemory (from :: SparseImageMemoryBind)) (vkMemoryOffset (from :: SparseImageMemoryBind)) (vkFlags (from :: SparseImageMemoryBind))))))
fromCStructSparseImageMemoryBind :: VkSparseImageMemoryBind -> IO SparseImageMemoryBind
fromCStructSparseImageMemoryBind c = SparseImageMemoryBind <$> (fromCStructImageSubresource (vkSubresource (c :: VkSparseImageMemoryBind)))
                                                           <*> (fromCStructOffset3D (vkOffset (c :: VkSparseImageMemoryBind)))
                                                           <*> (fromCStructExtent3D (vkExtent (c :: VkSparseImageMemoryBind)))
                                                           <*> pure (vkMemory (c :: VkSparseImageMemoryBind))
                                                           <*> pure (vkMemoryOffset (c :: VkSparseImageMemoryBind))
                                                           <*> pure (vkFlags (c :: VkSparseImageMemoryBind))
-- No documentation found for TopLevel "SparseImageMemoryBindInfo"
data SparseImageMemoryBindInfo = SparseImageMemoryBindInfo
  { -- No documentation found for Nested "SparseImageMemoryBindInfo" "image"
  vkImage :: Image
  -- Length valued member elided
  , -- No documentation found for Nested "SparseImageMemoryBindInfo" "pBinds"
  vkPBinds :: Vector SparseImageMemoryBind
  }
  deriving (Show, Eq)
withCStructSparseImageMemoryBindInfo :: SparseImageMemoryBindInfo -> (VkSparseImageMemoryBindInfo -> IO a) -> IO a
withCStructSparseImageMemoryBindInfo from cont = withVec withCStructSparseImageMemoryBind (vkPBinds (from :: SparseImageMemoryBindInfo)) (\pBinds -> cont (VkSparseImageMemoryBindInfo (vkImage (from :: SparseImageMemoryBindInfo)) (fromIntegral (Data.Vector.length (vkPBinds (from :: SparseImageMemoryBindInfo)))) pBinds))
fromCStructSparseImageMemoryBindInfo :: VkSparseImageMemoryBindInfo -> IO SparseImageMemoryBindInfo
fromCStructSparseImageMemoryBindInfo c = SparseImageMemoryBindInfo <$> pure (vkImage (c :: VkSparseImageMemoryBindInfo))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkBindCount (c :: VkSparseImageMemoryBindInfo))) (((fromCStructSparseImageMemoryBind <=<) . peekElemOff) (vkPBinds (c :: VkSparseImageMemoryBindInfo))))
-- No documentation found for TopLevel "SparseImageMemoryRequirements"
data SparseImageMemoryRequirements = SparseImageMemoryRequirements
  { -- No documentation found for Nested "SparseImageMemoryRequirements" "formatProperties"
  vkFormatProperties :: SparseImageFormatProperties
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailFirstLod"
  vkImageMipTailFirstLod :: Word32
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailSize"
  vkImageMipTailSize :: DeviceSize
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailOffset"
  vkImageMipTailOffset :: DeviceSize
  , -- No documentation found for Nested "SparseImageMemoryRequirements" "imageMipTailStride"
  vkImageMipTailStride :: DeviceSize
  }
  deriving (Show, Eq)
withCStructSparseImageMemoryRequirements :: SparseImageMemoryRequirements -> (VkSparseImageMemoryRequirements -> IO a) -> IO a
withCStructSparseImageMemoryRequirements from cont = withCStructSparseImageFormatProperties (vkFormatProperties (from :: SparseImageMemoryRequirements)) (\formatProperties -> cont (VkSparseImageMemoryRequirements formatProperties (vkImageMipTailFirstLod (from :: SparseImageMemoryRequirements)) (vkImageMipTailSize (from :: SparseImageMemoryRequirements)) (vkImageMipTailOffset (from :: SparseImageMemoryRequirements)) (vkImageMipTailStride (from :: SparseImageMemoryRequirements))))
fromCStructSparseImageMemoryRequirements :: VkSparseImageMemoryRequirements -> IO SparseImageMemoryRequirements
fromCStructSparseImageMemoryRequirements c = SparseImageMemoryRequirements <$> (fromCStructSparseImageFormatProperties (vkFormatProperties (c :: VkSparseImageMemoryRequirements)))
                                                                           <*> pure (vkImageMipTailFirstLod (c :: VkSparseImageMemoryRequirements))
                                                                           <*> pure (vkImageMipTailSize (c :: VkSparseImageMemoryRequirements))
                                                                           <*> pure (vkImageMipTailOffset (c :: VkSparseImageMemoryRequirements))
                                                                           <*> pure (vkImageMipTailStride (c :: VkSparseImageMemoryRequirements))
-- No documentation found for TopLevel "SparseImageOpaqueMemoryBindInfo"
data SparseImageOpaqueMemoryBindInfo = SparseImageOpaqueMemoryBindInfo
  { -- No documentation found for Nested "SparseImageOpaqueMemoryBindInfo" "image"
  vkImage :: Image
  -- Length valued member elided
  , -- No documentation found for Nested "SparseImageOpaqueMemoryBindInfo" "pBinds"
  vkPBinds :: Vector SparseMemoryBind
  }
  deriving (Show, Eq)
withCStructSparseImageOpaqueMemoryBindInfo :: SparseImageOpaqueMemoryBindInfo -> (VkSparseImageOpaqueMemoryBindInfo -> IO a) -> IO a
withCStructSparseImageOpaqueMemoryBindInfo from cont = withVec withCStructSparseMemoryBind (vkPBinds (from :: SparseImageOpaqueMemoryBindInfo)) (\pBinds -> cont (VkSparseImageOpaqueMemoryBindInfo (vkImage (from :: SparseImageOpaqueMemoryBindInfo)) (fromIntegral (Data.Vector.length (vkPBinds (from :: SparseImageOpaqueMemoryBindInfo)))) pBinds))
fromCStructSparseImageOpaqueMemoryBindInfo :: VkSparseImageOpaqueMemoryBindInfo -> IO SparseImageOpaqueMemoryBindInfo
fromCStructSparseImageOpaqueMemoryBindInfo c = SparseImageOpaqueMemoryBindInfo <$> pure (vkImage (c :: VkSparseImageOpaqueMemoryBindInfo))
                                                                               -- Length valued member elided
                                                                               <*> (Data.Vector.generateM (fromIntegral (vkBindCount (c :: VkSparseImageOpaqueMemoryBindInfo))) (((fromCStructSparseMemoryBind <=<) . peekElemOff) (vkPBinds (c :: VkSparseImageOpaqueMemoryBindInfo))))
-- No documentation found for TopLevel "SparseMemoryBind"
data SparseMemoryBind = SparseMemoryBind
  { -- No documentation found for Nested "SparseMemoryBind" "resourceOffset"
  vkResourceOffset :: DeviceSize
  , -- No documentation found for Nested "SparseMemoryBind" "size"
  vkSize :: DeviceSize
  , -- No documentation found for Nested "SparseMemoryBind" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "SparseMemoryBind" "memoryOffset"
  vkMemoryOffset :: DeviceSize
  , -- No documentation found for Nested "SparseMemoryBind" "flags"
  vkFlags :: SparseMemoryBindFlags
  }
  deriving (Show, Eq)
withCStructSparseMemoryBind :: SparseMemoryBind -> (VkSparseMemoryBind -> IO a) -> IO a
withCStructSparseMemoryBind from cont = cont (VkSparseMemoryBind (vkResourceOffset (from :: SparseMemoryBind)) (vkSize (from :: SparseMemoryBind)) (vkMemory (from :: SparseMemoryBind)) (vkMemoryOffset (from :: SparseMemoryBind)) (vkFlags (from :: SparseMemoryBind)))
fromCStructSparseMemoryBind :: VkSparseMemoryBind -> IO SparseMemoryBind
fromCStructSparseMemoryBind c = SparseMemoryBind <$> pure (vkResourceOffset (c :: VkSparseMemoryBind))
                                                 <*> pure (vkSize (c :: VkSparseMemoryBind))
                                                 <*> pure (vkMemory (c :: VkSparseMemoryBind))
                                                 <*> pure (vkMemoryOffset (c :: VkSparseMemoryBind))
                                                 <*> pure (vkFlags (c :: VkSparseMemoryBind))
-- No documentation found for TopLevel "SparseMemoryBindFlagBits"
type SparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits
-- No documentation found for TopLevel "SparseMemoryBindFlags"
type SparseMemoryBindFlags = SparseMemoryBindFlagBits

-- | Wrapper for 'vkGetImageSparseMemoryRequirements'
getNumImageSparseMemoryRequirements :: Device ->  Image ->  IO (Word32)
getNumImageSparseMemoryRequirements = \(Device device commandTable) -> \image -> alloca (\pSparseMemoryRequirementCount -> Graphics.Vulkan.C.Dynamic.getImageSparseMemoryRequirements commandTable device image pSparseMemoryRequirementCount nullPtr *> (peek pSparseMemoryRequirementCount))

-- | Wrapper for 'vkGetImageSparseMemoryRequirements'
getImageSparseMemoryRequirements :: Device ->  Image ->  Word32 ->  IO (Vector SparseImageMemoryRequirements)
getImageSparseMemoryRequirements = \(Device device commandTable) -> \image -> \sparseMemoryRequirementCount -> allocaArray (fromIntegral sparseMemoryRequirementCount) (\pSparseMemoryRequirements -> with sparseMemoryRequirementCount (\pSparseMemoryRequirementCount -> Graphics.Vulkan.C.Dynamic.getImageSparseMemoryRequirements commandTable device image pSparseMemoryRequirementCount pSparseMemoryRequirements *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageMemoryRequirements <=< peekElemOff p) pSparseMemoryRequirements) =<< (fromIntegral <$> (peek pSparseMemoryRequirementCount))))))
-- | Call 'getNumImageSparseMemoryRequirements' to get the number of return values, then use that
-- number to call 'getImageSparseMemoryRequirements' to get all the values.
getAllImageSparseMemoryRequirements :: Device ->  Image ->  IO (Vector SparseImageMemoryRequirements)
getAllImageSparseMemoryRequirements device image =
  getNumImageSparseMemoryRequirements device image
    >>= \num -> getImageSparseMemoryRequirements device image num


-- | Wrapper for 'vkGetPhysicalDeviceSparseImageFormatProperties'
getNumPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  IO ( Word32 )
getNumPhysicalDeviceSparseImageFormatProperties = \(PhysicalDevice physicalDevice commandTable) -> \format -> \type' -> \samples -> \usage -> \tiling -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSparseImageFormatProperties commandTable physicalDevice format type' samples usage tiling pPropertyCount nullPtr *> (peek pPropertyCount))

-- | Wrapper for 'vkGetPhysicalDeviceSparseImageFormatProperties'
getPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  Word32 ->  IO ( Vector SparseImageFormatProperties )
getPhysicalDeviceSparseImageFormatProperties = \(PhysicalDevice physicalDevice commandTable) -> \format -> \type' -> \samples -> \usage -> \tiling -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSparseImageFormatProperties commandTable physicalDevice format type' samples usage tiling pPropertyCount pProperties *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageFormatProperties <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount))))))
-- | Call 'getNumPhysicalDeviceSparseImageFormatProperties' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceSparseImageFormatProperties' to get all the values.
getAllPhysicalDeviceSparseImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  SampleCountFlagBits ->  ImageUsageFlags ->  ImageTiling ->  IO (Vector SparseImageFormatProperties)
getAllPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling =
  getNumPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling
    >>= \num -> getPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling num


-- | Wrapper for 'vkQueueBindSparse'
queueBindSparse :: Queue ->  Vector BindSparseInfo ->  Fence ->  IO ()
queueBindSparse = \(Queue queue commandTable) -> \bindInfo -> \fence -> withVec withCStructBindSparseInfo bindInfo (\pBindInfo -> Graphics.Vulkan.C.Dynamic.queueBindSparse commandTable queue (fromIntegral $ Data.Vector.length bindInfo) pBindInfo fence >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
