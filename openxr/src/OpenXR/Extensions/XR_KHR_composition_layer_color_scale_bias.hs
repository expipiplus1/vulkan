{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_color_scale_bias - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_color_scale_bias  XR_KHR_composition_layer_color_scale_bias>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 35
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
-- 'CompositionLayerColorScaleBiasKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_color_scale_bias OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_color_scale_bias  ( CompositionLayerColorScaleBiasKHR(..)
                                                                    , KHR_composition_layer_color_scale_bias_SPEC_VERSION
                                                                    , pattern KHR_composition_layer_color_scale_bias_SPEC_VERSION
                                                                    , KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME
                                                                    , pattern KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME
                                                                    ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Core10.OtherTypes (Color4f)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR))
-- | XrCompositionLayerColorScaleBiasKHR - defines color scale and bias for
-- layer textures
--
-- == Member Descriptions
--
-- = Description
--
-- 'CompositionLayerColorScaleBiasKHR' contains the information needed to
-- scale and bias the color of layer textures.
--
-- The 'CompositionLayerColorScaleBiasKHR' structure /can/ be applied by
-- applications to composition layers by adding an instance of the struct
-- to the 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader'::@next@
-- list.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerColorScaleBiasKHR-extension-notenabled# The
--     @@ extension /must/ be enabled prior to using
--     'CompositionLayerColorScaleBiasKHR'
--
-- -   #VUID-XrCompositionLayerColorScaleBiasKHR-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR'
--
-- -   #VUID-XrCompositionLayerColorScaleBiasKHR-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.Color4f',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data CompositionLayerColorScaleBiasKHR = CompositionLayerColorScaleBiasKHR
  { -- | @colorScale@ is an 'OpenXR.Core10.OtherTypes.Color4f' which will
    -- modulate the color sourced from the images.
    colorScale :: Color4f
  , -- | @colorBias@ is an 'OpenXR.Core10.OtherTypes.Color4f' which will offset
    -- the color sourced from the images.
    colorBias :: Color4f
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerColorScaleBiasKHR)
#endif
deriving instance Show CompositionLayerColorScaleBiasKHR

instance ToCStruct CompositionLayerColorScaleBiasKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerColorScaleBiasKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Color4f)) (colorScale)
    poke ((p `plusPtr` 32 :: Ptr Color4f)) (colorBias)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Color4f)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Color4f)) (zero)
    f

instance FromCStruct CompositionLayerColorScaleBiasKHR where
  peekCStruct p = do
    colorScale <- peekCStruct @Color4f ((p `plusPtr` 16 :: Ptr Color4f))
    colorBias <- peekCStruct @Color4f ((p `plusPtr` 32 :: Ptr Color4f))
    pure $ CompositionLayerColorScaleBiasKHR
             colorScale colorBias

instance Storable CompositionLayerColorScaleBiasKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerColorScaleBiasKHR where
  zero = CompositionLayerColorScaleBiasKHR
           zero
           zero


type KHR_composition_layer_color_scale_bias_SPEC_VERSION = 5

-- No documentation found for TopLevel "XR_KHR_composition_layer_color_scale_bias_SPEC_VERSION"
pattern KHR_composition_layer_color_scale_bias_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_composition_layer_color_scale_bias_SPEC_VERSION = 5


type KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME = "XR_KHR_composition_layer_color_scale_bias"

-- No documentation found for TopLevel "XR_KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME"
pattern KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME = "XR_KHR_composition_layer_color_scale_bias"

