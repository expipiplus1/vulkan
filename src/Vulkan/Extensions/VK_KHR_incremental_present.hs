{-# language CPP #-}
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_REGIONS_KHR))
-- | VkPresentRegionsKHR - Structure hint of rectangular regions changed by
-- vkQueuePresentKHR
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ be the same value as
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@,
--     where 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR' is
--     included in the @pNext@ chain of this 'PresentRegionsKHR' structure
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_REGIONS_KHR'
--
-- -   If @pRegions@ is not @NULL@, @pRegions@ /must/ be a valid pointer to
--     an array of @swapchainCount@ valid 'PresentRegionKHR' structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
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
-- -   If @rectangleCount@ is not @0@, and @pRectangles@ is not @NULL@,
--     @pRectangles@ /must/ be a valid pointer to an array of
--     @rectangleCount@ valid 'RectLayerKHR' structures
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
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRectangles `plusPtr` (20 * (i)) :: Ptr RectLayerKHR) (e) . ($ ())) ((rectangles))
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
-- -   The sum of @offset@ and @extent@ /must/ be no greater than the
--     @imageExtent@ member of the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
--     structure passed to
--     'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'
--
-- -   @layer@ /must/ be less than the @imageArrayLayers@ member of the
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
  pokeCStruct p RectLayerKHR{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr Offset2D)) (offset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr Extent2D)) (extent) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (layer)
    lift $ f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr Offset2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct RectLayerKHR where
  peekCStruct p = do
    offset <- peekCStruct @Offset2D ((p `plusPtr` 0 :: Ptr Offset2D))
    extent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    layer <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ RectLayerKHR
             offset extent layer

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

