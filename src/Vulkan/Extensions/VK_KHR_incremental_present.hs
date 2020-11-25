{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_incremental_present"
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Offset2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_REGIONS_KHR))

-- No documentation found for TopLevel "VkPresentRegionsKHR"
data PresentRegionsKHR = PresentRegionsKHR
  { -- No documentation found for Nested "VkPresentRegionsKHR" "swapchainCount"
    swapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentRegionsKHR" "pRegions"
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



-- No documentation found for TopLevel "VkPresentRegionKHR"
data PresentRegionKHR = PresentRegionKHR
  { -- No documentation found for Nested "VkPresentRegionKHR" "rectangleCount"
    rectangleCount :: Word32
  , -- No documentation found for Nested "VkPresentRegionKHR" "pRectangles"
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



-- No documentation found for TopLevel "VkRectLayerKHR"
data RectLayerKHR = RectLayerKHR
  { -- No documentation found for Nested "VkRectLayerKHR" "offset"
    offset :: Offset2D
  , -- No documentation found for Nested "VkRectLayerKHR" "extent"
    extent :: Extent2D
  , -- No documentation found for Nested "VkRectLayerKHR" "layer"
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

