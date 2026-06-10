{-| GPU render targets for the offscreen examples: a colour target (colour
attachment + transfer source, so it can be read back) and a depth attachment,
each a VMA-allocated GPU-only image with a matching whole-image 2D view.
-}
module RenderTarget
  ( createColorTarget
  , createDepthTarget
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Debug (nameObject)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

{- | A GPU-only colour render target (colour attachment + transfer source) with
a whole-image 2D view, named @\"GPU render target\"@. The keys release the image
and view ahead of the enclosing 'Control.Monad.Trans.Resource.ResourceT' if
needed.
-}
createColorTarget
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Vk.Extent2D
  -> m ((ReleaseKey, ReleaseKey), (Vk.Image, Vk.ImageView))
createColorTarget allocator dev format extent =
  createTarget
    allocator
    dev
    format
    extent
    (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT)
    Vk.IMAGE_ASPECT_COLOR_BIT
    "GPU render target"

{- | A GPU-only depth attachment with a whole-image 2D depth-aspect view, named
@\"GPU depth attachment\"@. Keys as in 'createColorTarget'.
-}
createDepthTarget
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Vk.Extent2D
  -> m ((ReleaseKey, ReleaseKey), (Vk.Image, Vk.ImageView))
createDepthTarget allocator dev format extent =
  createTarget
    allocator
    dev
    format
    extent
    Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
    Vk.IMAGE_ASPECT_DEPTH_BIT
    "GPU depth attachment"

createTarget
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Vk.Extent2D
  -> Vk.ImageUsageFlags
  -> Vk.ImageAspectFlags
  -> ByteString
  -> m ((ReleaseKey, ReleaseKey), (Vk.Image, Vk.ImageView))
createTarget allocator dev format (Vk.Extent2D w h) usage aspect name = do
  (imageKey, (image, _, _)) <- VMA.withImage allocator imageCreateInfo gpuAlloc allocate
  nameObject dev image name
  (viewKey, view) <- Vk.withImageView dev (viewCreateInfo image) Nothing allocate
  pure ((imageKey, viewKey), (image, view))
  where
    gpuAlloc = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
    imageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = format
        , Vk.extent = Vk.Extent3D w h 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = 1
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage = usage
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
    viewCreateInfo image =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange aspect 0 1 0 1
        }
