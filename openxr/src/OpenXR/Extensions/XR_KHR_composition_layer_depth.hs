{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_depth - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_depth  XR_KHR_composition_layer_depth>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 11
--
-- = Revision
--
-- 5
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'CompositionLayerDepthInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_depth OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_depth  ( CompositionLayerDepthInfoKHR(..)
                                                         , KHR_composition_layer_depth_SPEC_VERSION
                                                         , pattern KHR_composition_layer_depth_SPEC_VERSION
                                                         , KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME
                                                         , pattern KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME
                                                         ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.OtherTypes (SwapchainSubImage)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR))
-- | XrCompositionLayerDepthInfoKHR - Depth map layer info
--
-- == Member Descriptions
--
-- = Description
--
-- 'CompositionLayerDepthInfoKHR' contains the information needed to
-- specify an extra layer with depth information. When submitting depth
-- buffers along with projection layers, add the
-- 'CompositionLayerDepthInfoKHR' to the @next@ chain for all
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjectionView' structures in
-- the given layer.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerDepthInfoKHR-extension-notenabled# The
--     @XR_KHR_composition_layer_depth@ extension /must/ be enabled prior
--     to using 'CompositionLayerDepthInfoKHR'
--
-- -   #VUID-XrCompositionLayerDepthInfoKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR'
--
-- -   #VUID-XrCompositionLayerDepthInfoKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrCompositionLayerDepthInfoKHR-subImage-parameter# @subImage@
--     /must/ be a valid 'OpenXR.Core10.OtherTypes.SwapchainSubImage'
--     structure
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjection',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjectionView',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.OtherTypes.SwapchainSubImage',
-- 'OpenXR.Core10.DisplayTiming.endFrame'
data CompositionLayerDepthInfoKHR = CompositionLayerDepthInfoKHR
  { -- | @subImage@ identifies the depth image
    -- 'OpenXR.Core10.OtherTypes.SwapchainSubImage' to be associated with the
    -- color swapchain. The contained @imageRect@ specifies the valid portion
    -- of the depth image to use, in pixels. It also implicitly defines the
    -- transform from normalized image coordinates into pixel coordinates. The
    -- contained @imageArrayIndex@ is the depth image array index, with 0
    -- meaning the first or only array element.
    subImage :: SwapchainSubImage
  , -- | @minDepth@ and @maxDepth@ are the range of depth values the
    -- @depthSwapchain@ could have, in the range of [0.0,1.0]. This is akin to
    -- min and max values of OpenGL’s @glDepthRange@, but with the requirement
    -- here that maxDepth ≥ minDepth.
    minDepth :: Float
  , -- No documentation found for Nested "XrCompositionLayerDepthInfoKHR" "maxDepth"
    maxDepth :: Float
  , -- | @nearZ@ is the positive distance in meters of the @minDepth@ value in
    -- the depth swapchain. Applications /may/ use a @nearZ@ that is greater
    -- than @farZ@ to indicate depth values are reversed. @nearZ@ can be
    -- infinite.
    nearZ :: Float
  , -- | @farZ@ is the positive distance in meters of the @maxDepth@ value in the
    -- depth swapchain. @farZ@ can be infinite. Applications /must/ not use the
    -- same value as @nearZ@.
    farZ :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerDepthInfoKHR)
#endif
deriving instance Show CompositionLayerDepthInfoKHR

instance ToCStruct CompositionLayerDepthInfoKHR where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerDepthInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainSubImage)) (subImage)
    poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (minDepth))
    poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (maxDepth))
    poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (nearZ))
    poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (farZ))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainSubImage)) (zero)
    poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 60 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct CompositionLayerDepthInfoKHR where
  peekCStruct p = do
    subImage <- peekCStruct @SwapchainSubImage ((p `plusPtr` 16 :: Ptr SwapchainSubImage))
    minDepth <- peek @CFloat ((p `plusPtr` 48 :: Ptr CFloat))
    maxDepth <- peek @CFloat ((p `plusPtr` 52 :: Ptr CFloat))
    nearZ <- peek @CFloat ((p `plusPtr` 56 :: Ptr CFloat))
    farZ <- peek @CFloat ((p `plusPtr` 60 :: Ptr CFloat))
    pure $ CompositionLayerDepthInfoKHR
             subImage
             (coerce @CFloat @Float minDepth)
             (coerce @CFloat @Float maxDepth)
             (coerce @CFloat @Float nearZ)
             (coerce @CFloat @Float farZ)

instance Storable CompositionLayerDepthInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerDepthInfoKHR where
  zero = CompositionLayerDepthInfoKHR
           zero
           zero
           zero
           zero
           zero


type KHR_composition_layer_depth_SPEC_VERSION = 5

-- No documentation found for TopLevel "XR_KHR_composition_layer_depth_SPEC_VERSION"
pattern KHR_composition_layer_depth_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_composition_layer_depth_SPEC_VERSION = 5


type KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME = "XR_KHR_composition_layer_depth"

-- No documentation found for TopLevel "XR_KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME"
pattern KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME = "XR_KHR_composition_layer_depth"

