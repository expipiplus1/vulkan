{-# language CPP #-}
-- | = Name
--
-- VK_KHR_incremental_present - device extension
--
-- == VK_KHR_incremental_present
--
-- [__Name String__]
--     @VK_KHR_incremental_present@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     85
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_incremental_present:%20&body=@ianelliottus%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-11-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   Jesse Hall, Google
--
--     -   Alon Or-bach, Samsung
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Ray Smith, ARM
--
--     -   Mika Isojarvi, Google
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This device extension extends
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', from the
-- @VK_KHR_swapchain@ extension, allowing an application to specify a list
-- of rectangular, modified regions of each image to present. This should
-- be used in situations where an application is only changing a small
-- portion of the presentable images within a swapchain, since it enables
-- the presentation engine to avoid wasting time presenting parts of the
-- surface that have not changed.
--
-- This extension is leveraged from the @EGL_KHR_swap_buffers_with_damage@
-- extension.
--
-- == New Structures
--
-- -   'PresentRegionKHR'
--
-- -   'RectLayerKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentRegionsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_INCREMENTAL_PRESENT_EXTENSION_NAME'
--
-- -   'KHR_INCREMENTAL_PRESENT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_REGIONS_KHR'
--
-- == Issues
--
-- 1) How should we handle steroescopic-3D swapchains? We need to add a
-- layer for each rectangle. One approach is to create another struct
-- containing the 'Vulkan.Core10.FundamentalTypes.Rect2D' plus layer, and
-- have 'PresentRegionsKHR' point to an array of that struct. Another
-- approach is to have two parallel arrays, @pRectangles@ and @pLayers@,
-- where @pRectangles@[i] and @pLayers@[i] must be used together. Which
-- approach should we use, and if the array of a new structure, what should
-- that be called?
--
-- __RESOLVED__: Create a new structure, which is a
-- 'Vulkan.Core10.FundamentalTypes.Rect2D' plus a layer, and will be called
-- 'RectLayerKHR'.
--
-- 2) Where is the origin of the 'RectLayerKHR'?
--
-- __RESOLVED__: The upper left corner of the presentable image(s) of the
-- swapchain, per the definition of framebuffer coordinates.
--
-- 3) Does the rectangular region, 'RectLayerKHR', specify pixels of the
-- swapchain’s image(s), or of the surface?
--
-- __RESOLVED__: Of the image(s). Some presentation engines may scale the
-- pixels of a swapchain’s image(s) to the size of the surface. The size of
-- the swapchain’s image(s) will be consistent, where the size of the
-- surface may vary over time.
--
-- 4) What if all of the rectangles for a given swapchain contain a width
-- and\/or height of zero?
--
-- __RESOLVED__: The application is indicating that no pixels changed since
-- the last present. The presentation engine may use such a hint and not
-- update any pixels for the swapchain. However, all other semantics of
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' must still be
-- honored, including waiting for semaphores to signal.
--
-- == Version History
--
-- -   Revision 1, 2016-11-02 (Ian Elliott)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PresentRegionKHR', 'PresentRegionsKHR', 'RectLayerKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_incremental_present Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_incremental_present  ( PresentRegionsKHR(..)
                                                     , PresentRegionKHR(..)
                                                     , RectLayerKHR(..)
                                                     , KHR_INCREMENTAL_PRESENT_SPEC_VERSION
                                                     , pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION
                                                     , KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
                                                     , pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
                                                     ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_REGIONS_KHR))
-- | VkPresentRegionsKHR - Structure hint of rectangular regions changed by
-- vkQueuePresentKHR
--
-- == Valid Usage
--
-- -   #VUID-VkPresentRegionsKHR-swapchainCount-01260# @swapchainCount@
--     /must/ be the same value as
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@,
--     where 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' is
--     included in the @pNext@ chain of this 'PresentRegionsKHR' structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentRegionsKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_REGIONS_KHR'
--
-- -   #VUID-VkPresentRegionsKHR-pRegions-parameter# If @pRegions@ is not
--     @NULL@, @pRegions@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid 'PresentRegionKHR' structures
--
-- -   #VUID-VkPresentRegionsKHR-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'PresentRegionKHR', 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentRegionsKHR = PresentRegionsKHR
  { -- | @swapchainCount@ is the number of swapchains being presented to by this
    -- command.
    swapchainCount :: Word32
  , -- | @pRegions@ is @NULL@ or a pointer to an array of 'PresentRegionKHR'
    -- elements with @swapchainCount@ entries. If not @NULL@, each element of
    -- @pRegions@ contains the region that has changed since the last present
    -- to the swapchain in the corresponding entry in the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pSwapchains@
    -- array.
    regions :: Vector PresentRegionKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentRegionsKHR)
#endif
deriving instance Show PresentRegionsKHR

instance ToCStruct PresentRegionsKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentRegionsKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_REGIONS_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pRegionsLength = Data.Vector.length $ (regions)
    swapchainCount'' <- lift $ if (swapchainCount) == 0
      then pure $ fromIntegral pRegionsLength
      else do
        unless (fromIntegral pRegionsLength == (swapchainCount) || pRegionsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pRegions must be empty or have 'swapchainCount' elements" Nothing Nothing
        pure (swapchainCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (swapchainCount'')
    pRegions'' <- if Data.Vector.null (regions)
      then pure nullPtr
      else do
        pPRegions <- ContT $ allocaBytesAligned @PresentRegionKHR (((Data.Vector.length (regions))) * 16) 8
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (16 * (i)) :: Ptr PresentRegionKHR) (e) . ($ ())) ((regions))
        pure $ pPRegions
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentRegionKHR))) pRegions''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_REGIONS_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PresentRegionsKHR where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pRegions <- peek @(Ptr PresentRegionKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentRegionKHR)))
    let pRegionsLength = if pRegions == nullPtr then 0 else (fromIntegral swapchainCount)
    pRegions' <- generateM pRegionsLength (\i -> peekCStruct @PresentRegionKHR ((pRegions `advancePtrBytes` (16 * (i)) :: Ptr PresentRegionKHR)))
    pure $ PresentRegionsKHR
             swapchainCount pRegions'

instance Zero PresentRegionsKHR where
  zero = PresentRegionsKHR
           zero
           mempty


-- | VkPresentRegionKHR - Structure containing rectangular region changed by
-- vkQueuePresentKHR for a given VkImage
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentRegionKHR-pRectangles-parameter# If @rectangleCount@
--     is not @0@, and @pRectangles@ is not @NULL@, @pRectangles@ /must/ be
--     a valid pointer to an array of @rectangleCount@ valid 'RectLayerKHR'
--     structures
--
-- = See Also
--
-- 'PresentRegionsKHR', 'RectLayerKHR'
data PresentRegionKHR = PresentRegionKHR
  { -- | @rectangleCount@ is the number of rectangles in @pRectangles@, or zero
    -- if the entire image has changed and should be presented.
    rectangleCount :: Word32
  , -- | @pRectangles@ is either @NULL@ or a pointer to an array of
    -- 'RectLayerKHR' structures. The 'RectLayerKHR' structure is the
    -- framebuffer coordinates, plus layer, of a portion of a presentable image
    -- that has changed and /must/ be presented. If non-@NULL@, each entry in
    -- @pRectangles@ is a rectangle of the given image that has changed since
    -- the last image was presented to the given swapchain.
    rectangles :: Vector RectLayerKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentRegionKHR)
#endif
deriving instance Show PresentRegionKHR

instance ToCStruct PresentRegionKHR where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentRegionKHR{..} f = evalContT $ do
    let pRectanglesLength = Data.Vector.length $ (rectangles)
    rectangleCount'' <- lift $ if (rectangleCount) == 0
      then pure $ fromIntegral pRectanglesLength
      else do
        unless (fromIntegral pRectanglesLength == (rectangleCount) || pRectanglesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pRectangles must be empty or have 'rectangleCount' elements" Nothing Nothing
        pure (rectangleCount)
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (rectangleCount'')
    pRectangles'' <- if Data.Vector.null (rectangles)
      then pure nullPtr
      else do
        pPRectangles <- ContT $ allocaBytesAligned @RectLayerKHR (((Data.Vector.length (rectangles))) * 20) 4
        lift $ Data.Vector.imapM_ (\i e -> poke (pPRectangles `plusPtr` (20 * (i)) :: Ptr RectLayerKHR) (e)) ((rectangles))
        pure $ pPRectangles
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr RectLayerKHR))) pRectangles''
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct PresentRegionKHR where
  peekCStruct p = do
    rectangleCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pRectangles <- peek @(Ptr RectLayerKHR) ((p `plusPtr` 8 :: Ptr (Ptr RectLayerKHR)))
    let pRectanglesLength = if pRectangles == nullPtr then 0 else (fromIntegral rectangleCount)
    pRectangles' <- generateM pRectanglesLength (\i -> peekCStruct @RectLayerKHR ((pRectangles `advancePtrBytes` (20 * (i)) :: Ptr RectLayerKHR)))
    pure $ PresentRegionKHR
             rectangleCount pRectangles'

instance Zero PresentRegionKHR where
  zero = PresentRegionKHR
           zero
           mempty


-- | VkRectLayerKHR - Structure containing a rectangle, including layer,
-- changed by vkQueuePresentKHR for a given VkImage
--
-- == Valid Usage
--
-- -   #VUID-VkRectLayerKHR-offset-01261# The sum of @offset@ and @extent@
--     /must/ be no greater than the @imageExtent@ member of the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
--     structure passed to
--     'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'
--
-- -   #VUID-VkRectLayerKHR-layer-01262# @layer@ /must/ be less than the
--     @imageArrayLayers@ member of the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
--     structure passed to
--     'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'
--
-- Some platforms allow the size of a surface to change, and then scale the
-- pixels of the image to fit the surface. 'RectLayerKHR' specifies pixels
-- of the swapchain’s image(s), which will be constant for the life of the
-- swapchain.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.FundamentalTypes.Offset2D', 'PresentRegionKHR'
data RectLayerKHR = RectLayerKHR
  { -- | @offset@ is the origin of the rectangle, in pixels.
    offset :: Offset2D
  , -- | @extent@ is the size of the rectangle, in pixels.
    extent :: Extent2D
  , -- | @layer@ is the layer of the image. For images with only one layer, the
    -- value of @layer@ /must/ be 0.
    layer :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RectLayerKHR)
#endif
deriving instance Show RectLayerKHR

instance ToCStruct RectLayerKHR where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RectLayerKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2D)) (offset)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (extent)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (layer)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2D)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct RectLayerKHR where
  peekCStruct p = do
    offset <- peekCStruct @Offset2D ((p `plusPtr` 0 :: Ptr Offset2D))
    extent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    layer <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ RectLayerKHR
             offset extent layer

instance Storable RectLayerKHR where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RectLayerKHR where
  zero = RectLayerKHR
           zero
           zero
           zero


type KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION"
pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1


type KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = "VK_KHR_incremental_present"

-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME"
pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = "VK_KHR_incremental_present"

