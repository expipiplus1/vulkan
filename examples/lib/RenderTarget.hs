{-| GPU render targets for the offscreen examples: a colour target (colour
attachment + transfer source, so it can be read back) and a depth attachment,
each a VMA-allocated GPU-only image with a matching whole-image 2D view.
-}
module RenderTarget
  ( createColorTarget
  , createDepthTarget
  , createTarget
  , createMipChain
  , createCubeArray
  , createArrayTarget
  , linearSampler
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word32)
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
    Nothing
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
    Nothing
    "GPU depth attachment"

{- | A GPU-only 2D image (+ whole-image 2D view) with the given usage and aspect.
@sharedFamilies@ makes it CONCURRENT across those queue families (for a
cross-queue read with no ownership transfer); 'Nothing' leaves it EXCLUSIVE.
-}
createTarget
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Vk.Extent2D
  -> Vk.ImageUsageFlags
  -> Vk.ImageAspectFlags
  -> Maybe [Word32]
  -- ^ Queue families to share the image across (CONCURRENT), or 'Nothing' (EXCLUSIVE).
  -> ByteString
  -> m ((ReleaseKey, ReleaseKey), (Vk.Image, Vk.ImageView))
createTarget allocator dev format (Vk.Extent2D w h) usage aspect sharedFamilies name = do
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
        , Vk.sharingMode = maybe Vk.SHARING_MODE_EXCLUSIVE (const Vk.SHARING_MODE_CONCURRENT) sharedFamilies
        , Vk.queueFamilyIndices = maybe V.empty V.fromList sharedFamilies
        }
    viewCreateInfo image =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange aspect 0 1 0 1
        }

{- | A GPU-only mipped 2D image (@mipLevels@ levels from the given base extent) plus
one single-level 2D colour view per mip. For a bloom pyramid: each view is bound as
both a storage image (write) and a sampled image (read), and imported into the frame
graph as its own subresource.
-}
createMipChain
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Vk.Extent2D
  -> Word32
  -> Vk.ImageUsageFlags
  -> ByteString
  -> m (ReleaseKey, (Vk.Image, V.Vector Vk.ImageView))
createMipChain allocator dev format (Vk.Extent2D w h) mipLevels usage name = do
  (imageKey, (image, _, _)) <- VMA.withImage allocator imageCreateInfo gpuAlloc allocate
  nameObject dev image name
  views <- V.generateM (fromIntegral mipLevels) \i -> snd <$> Vk.withImageView dev (viewCreateInfo image (fromIntegral i)) Nothing allocate
  pure (imageKey, (image, views))
  where
    gpuAlloc = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
    imageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = format
        , Vk.extent = Vk.Extent3D w h 1
        , Vk.mipLevels = mipLevels
        , Vk.arrayLayers = 1
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage = usage
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
        }
    viewCreateInfo image level =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT level 1 0 1
        }

{- | A cube-map array (@nCubes@ cubes = @nCubes*6@ layers) plus a @CUBE_ARRAY@
sampled view of the whole thing and one @2D_ARRAY@ render view per cube (its six
faces, for a multiview render). Square @res@ per face; needs the @imageCubeArray@
device feature to sample.
-}
createCubeArray
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Word32
  -- ^ face resolution (square)
  -> Word32
  -- ^ number of cubes
  -> Vk.ImageUsageFlags
  -> Maybe [Word32]
  -- ^ queue families to share across (CONCURRENT, for a cross-queue read), or 'Nothing'
  -> ByteString
  -> m (ReleaseKey, (Vk.Image, Vk.ImageView, V.Vector Vk.ImageView))
createCubeArray allocator dev format res nCubes usage sharedFamilies name = do
  (imageKey, (image, _, _)) <- VMA.withImage allocator imageCreateInfo gpuAlloc allocate
  nameObject dev image name
  (_, cubeView) <- Vk.withImageView dev (cubeViewInfo image) Nothing allocate
  renderViews <- V.generateM (fromIntegral nCubes) \i -> snd <$> Vk.withImageView dev (renderViewInfo image (fromIntegral i * 6)) Nothing allocate
  pure (imageKey, (image, cubeView, renderViews))
  where
    gpuAlloc = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
    imageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = format
        , Vk.extent = Vk.Extent3D res res 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = nCubes * 6
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage = usage
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.flags = Vk.IMAGE_CREATE_CUBE_COMPATIBLE_BIT
        , Vk.sharingMode = maybe Vk.SHARING_MODE_EXCLUSIVE (const Vk.SHARING_MODE_CONCURRENT) sharedFamilies
        , Vk.queueFamilyIndices = maybe V.empty V.fromList sharedFamilies
        }
    cubeViewInfo image =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_CUBE_ARRAY
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 (nCubes * 6)
        }
    renderViewInfo image base =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D_ARRAY
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT 0 1 base 6
        }

{- | A GPU-only 2D-array image (@layers@ layers) with a single @2D_ARRAY@ view —
e.g. a multiview depth cube (6 layers). EXCLUSIVE.
-}
createArrayTarget
  :: (MonadResource m)
  => VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -> Word32
  -- ^ square resolution
  -> Word32
  -- ^ layer count
  -> Vk.ImageUsageFlags
  -> Vk.ImageAspectFlags
  -> ByteString
  -> m (ReleaseKey, (Vk.Image, Vk.ImageView))
createArrayTarget allocator dev format res layers usage aspect name = do
  (imageKey, (image, _, _)) <- VMA.withImage allocator imageCreateInfo gpuAlloc allocate
  nameObject dev image name
  (_, view) <- Vk.withImageView dev (viewCreateInfo image) Nothing allocate
  pure (imageKey, (image, view))
  where
    gpuAlloc = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
    imageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = format
        , Vk.extent = Vk.Extent3D res res 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = layers
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage = usage
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
        }
    viewCreateInfo image =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D_ARRAY
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange aspect 0 1 0 layers
        }

-- | A linear, clamp-to-edge sampler (bilinear taps for the bloom down/upsample).
linearSampler :: (MonadResource m) => Vk.Device -> m (ReleaseKey, Vk.Sampler)
linearSampler dev = Vk.withSampler dev info Nothing allocate
  where
    info =
      zero
        { Vk.magFilter = Vk.FILTER_LINEAR
        , Vk.minFilter = Vk.FILTER_LINEAR
        , Vk.mipmapMode = Vk.SAMPLER_MIPMAP_MODE_NEAREST
        , Vk.addressModeU = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        , Vk.addressModeV = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        , Vk.addressModeW = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        }
