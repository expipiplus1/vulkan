{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

{-| Helpers for the headless examples: allocate a CPU-mapped readback image,
copy a rendered colour image into it, turn it into a 'JP.Image', and write PNG.
-}
module ImageReadback
  ( makeReadbackImage
  , copyImageToHost
  , captureImageRGBA8
  , savePng
  ) where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, allocate)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Data.Word (Word32)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, sizeOf)
import Say (sayErr)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as ImageMemoryBarrier (ImageMemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.Image as Image
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

{- | Allocate a CPU-visible, mapped, linear @TRANSFER_DST@ image of the given
colour format and extent (the destination for 'copyImageToHost'), and return it
alongside a readback action that materializes its current contents as a
'JP.Image'. The image lives until the enclosing 'Control.Monad.Trans.Resource.ResourceT'
releases it.
-}
makeReadbackImage
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Vk.Extent2D
  -> m (Vk.Image, m (JP.Image JP.PixelRGBA8))
makeReadbackImage allocator dev format (Vk.Extent2D w h) = do
  (_, (cpuImage, allocation, allocationInfo)) <-
    VMA.withImage allocator cpuImageCreateInfo cpuAlloc allocate
  layout <-
    Image.getImageSubresourceLayout dev cpuImage (Vk.ImageSubresource Vk.IMAGE_ASPECT_COLOR_BIT 0 0)
  let
    pixelAddr :: Int -> Int -> Ptr Word32
    pixelAddr x y =
      plusPtr
        (VMA.mappedData allocationInfo)
        ( fromIntegral (Image.offset layout)
            + y * fromIntegral (Image.rowPitch layout)
            + x * sizeOf (0 :: Word32)
        )
    readback =
      captureImageRGBA8
        allocator
        allocation
        (fromIntegral w)
        (fromIntegral h)
        (\x y -> JP.unpackPixel @JP.PixelRGBA8 <$> peek (pixelAddr x y))
  pure (cpuImage, readback)
  where
    cpuImageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = format
        , Vk.extent = Vk.Extent3D w h 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = 1
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_LINEAR
        , Vk.usage = Vk.IMAGE_USAGE_TRANSFER_DST_BIT
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
    cpuAlloc =
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
        }

{- | Record the copy of a freshly rendered colour @image@ (left in
@COLOR_ATTACHMENT_OPTIMAL@) into a CPU readback @cpuImage@ (from
'makeReadbackImage'): barrier the source to a transfer source, the destination to
a transfer destination, copy, then make the destination host-readable.
-}
copyImageToHost
  :: (MonadIO m)
  => Vk.CommandBuffer
  -> Vk.Extent2D
  -> Vk.Image
  -> Vk.Image
  -> m ()
copyImageToHost cb (Vk.Extent2D w h) image cpuImage = do
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        Vk.ACCESS_TRANSFER_READ_BIT
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        image
    ]
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    []
    []
    [ imageBarrier
        zero
        Vk.ACCESS_TRANSFER_WRITE_BIT
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        cpuImage
    ]
  let layers = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1
  Vk.cmdCopyImage
    cb
    image
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    cpuImage
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    [ Vk.ImageCopy
        { srcSubresource = layers
        , srcOffset = Vk.Offset3D 0 0 0
        , dstSubresource = layers
        , dstOffset = Vk.Offset3D 0 0 0
        , extent = Vk.Extent3D w h 1
        }
    ]
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_HOST_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.ACCESS_TRANSFER_WRITE_BIT
        Vk.ACCESS_HOST_READ_BIT
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        Vk.IMAGE_LAYOUT_GENERAL
        cpuImage
    ]

-- | A whole-colour-image 'Vk.ImageMemoryBarrier' wrapped for 'Vk.cmdPipelineBarrier'.
imageBarrier
  :: Vk.AccessFlags
  -> Vk.AccessFlags
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> Vk.Image
  -> SomeStruct Vk.ImageMemoryBarrier
imageBarrier srcAccess dstAccess oldLayout newLayout img =
  SomeStruct
    zero
      { ImageMemoryBarrier.srcAccessMask = srcAccess
      , ImageMemoryBarrier.dstAccessMask = dstAccess
      , ImageMemoryBarrier.oldLayout = oldLayout
      , ImageMemoryBarrier.newLayout = newLayout
      , ImageMemoryBarrier.image = img
      , ImageMemoryBarrier.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
      }

{- | Invalidate the allocation (for non-HOST_COHERENT memory) then materialize
a JuicyPixels image from the caller's per-pixel reader.
-}
captureImageRGBA8
  :: (MonadIO m)
  => VMA.Allocator
  -> VMA.Allocation
  -> Int
  -> Int
  -> (Int -> Int -> IO JP.PixelRGBA8)
  -> m (JP.Image JP.PixelRGBA8)
captureImageRGBA8 allocator allocation width height reader = do
  VMA.invalidateAllocation allocator allocation 0 Vk.WHOLE_SIZE
  liftIO (JP.withImage width height reader)

-- | @sayErr "Writing PATH"@ then write the image as PNG.
savePng :: (MonadIO m) => FilePath -> JP.Image JP.PixelRGBA8 -> m ()
savePng path img = do
  sayErr $ "Writing " <> Text.pack path
  liftIO $ BSL.writeFile path (JP.encodePng img)
