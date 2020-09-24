{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_portability_subset  ( PhysicalDevicePortabilitySubsetFeaturesKHR(..)
                                                    , PhysicalDevicePortabilitySubsetPropertiesKHR(..)
                                                    , KHR_PORTABILITY_SUBSET_SPEC_VERSION
                                                    , pattern KHR_PORTABILITY_SUBSET_SPEC_VERSION
                                                    , KHR_PORTABILITY_SUBSET_EXTENSION_NAME
                                                    , pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME
                                                    ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR))
-- | VkPhysicalDevicePortabilitySubsetFeaturesKHR - Structure describing the
-- features that may not be supported by an implementation of the Vulkan
-- 1.0 Portability Subset
--
-- = Members
--
-- The members of the 'PhysicalDevicePortabilitySubsetFeaturesKHR'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePortabilitySubsetFeaturesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the features are supported.
-- 'PhysicalDevicePortabilitySubsetFeaturesKHR' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePortabilitySubsetFeaturesKHR = PhysicalDevicePortabilitySubsetFeaturesKHR
  { -- | @constantAlphaColorBlendFactors@ indicates whether this implementation
    -- supports constant /alpha/
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blendfactors>
    -- used as source or destination /color/
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blending>.
    constantAlphaColorBlendFactors :: Bool
  , -- | @events@ indicates whether this implementation supports synchronization
    -- using
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-events>.
    events :: Bool
  , -- | @imageViewFormatReinterpretation@ indicates whether this implementation
    -- supports a 'Vulkan.Core10.Handles.ImageView' being created with a texel
    -- format containing a different number of components, or a different
    -- number of bits in each component, than the texel format of the
    -- underlying 'Vulkan.Core10.Handles.Image'.
    imageViewFormatReinterpretation :: Bool
  , -- | @imageViewFormatSwizzle@ indicates whether this implementation supports
    -- remapping format components using
    -- 'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@components@.
    imageViewFormatSwizzle :: Bool
  , -- | @imageView2DOn3DImage@ indicates whether this implementation supports a
    -- 'Vulkan.Core10.Handles.Image' being created with the
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
    -- flag set, permitting a 2D or 2D array image view to be created on a 3D
    -- 'Vulkan.Core10.Handles.Image'.
    imageView2DOn3DImage :: Bool
  , -- | @multisampleArrayImage@ indicates whether this implementation supports a
    -- 'Vulkan.Core10.Handles.Image' being created as a 2D array with multiple
    -- samples per texel.
    multisampleArrayImage :: Bool
  , -- | @mutableComparisonSamplers@ indicates whether this implementation allows
    -- descriptors with comparison samplers to be
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-updates updated>.
    mutableComparisonSamplers :: Bool
  , -- | @pointPolygons@ indicates whether this implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast>
    -- using a /point/
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-polygonmode>.
    pointPolygons :: Bool
  , -- | @samplerMipLodBias@ indicates whether this implementation supports
    -- setting a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-mipLodBias mipmap LOD bias value>
    -- when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers creating a sampler>.
    samplerMipLodBias :: Bool
  , -- | @separateStencilMaskRef@ indicates whether this implementation supports
    -- separate front and back
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil>
    -- reference values.
    separateStencilMaskRef :: Bool
  , -- | @shaderSampleRateInterpolationFunctions@ indicates whether this
    -- implementation supports fragment shaders which use the
    -- @InterpolationFunction@
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table capability>
    -- and the extended instructions @InterpolateAtCentroid@,
    -- @InterpolateAtOffset@, and @InterpolateAtSample@ from the @GLSL.std.450@
    -- extended instruction set. This member is only meaningful if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sampleRateShading sampleRateShading>
    -- feature is supported.
    shaderSampleRateInterpolationFunctions :: Bool
  , -- | @tessellationIsolines@ indicates whether this implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation-isoline-tessellation isoline output>
    -- from the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation>
    -- stage of a graphics pipeline. This member is only meaningful if
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
    -- are supported.
    tessellationIsolines :: Bool
  , -- | @tessellationPointMode@ indicates whether this implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation-point-mode point output>
    -- from the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation>
    -- stage of a graphics pipeline. This member is only meaningful if
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
    -- are supported.
    tessellationPointMode :: Bool
  , -- | @triangleFans@ indicates whether this implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-triangle-fans>
    -- primitive topology.
    triangleFans :: Bool
  , -- | @vertexAttributeAccessBeyondStride@ indicates whether this
    -- implementation supports accessing a vertex input attribute beyond the
    -- stride of the corresponding vertex input binding.
    vertexAttributeAccessBeyondStride :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePortabilitySubsetFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePortabilitySubsetFeaturesKHR

instance ToCStruct PhysicalDevicePortabilitySubsetFeaturesKHR where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePortabilitySubsetFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (constantAlphaColorBlendFactors))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (events))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (imageViewFormatReinterpretation))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (imageViewFormatSwizzle))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (imageView2DOn3DImage))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (multisampleArrayImage))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (mutableComparisonSamplers))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (pointPolygons))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (samplerMipLodBias))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (separateStencilMaskRef))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (shaderSampleRateInterpolationFunctions))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (tessellationIsolines))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (tessellationPointMode))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (triangleFans))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (vertexAttributeAccessBeyondStride))
    f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePortabilitySubsetFeaturesKHR where
  peekCStruct p = do
    constantAlphaColorBlendFactors <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    events <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    imageViewFormatReinterpretation <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    imageViewFormatSwizzle <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    imageView2DOn3DImage <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    multisampleArrayImage <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    mutableComparisonSamplers <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pointPolygons <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    samplerMipLodBias <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    separateStencilMaskRef <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    shaderSampleRateInterpolationFunctions <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    tessellationIsolines <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    tessellationPointMode <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    triangleFans <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    vertexAttributeAccessBeyondStride <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    pure $ PhysicalDevicePortabilitySubsetFeaturesKHR
             (bool32ToBool constantAlphaColorBlendFactors) (bool32ToBool events) (bool32ToBool imageViewFormatReinterpretation) (bool32ToBool imageViewFormatSwizzle) (bool32ToBool imageView2DOn3DImage) (bool32ToBool multisampleArrayImage) (bool32ToBool mutableComparisonSamplers) (bool32ToBool pointPolygons) (bool32ToBool samplerMipLodBias) (bool32ToBool separateStencilMaskRef) (bool32ToBool shaderSampleRateInterpolationFunctions) (bool32ToBool tessellationIsolines) (bool32ToBool tessellationPointMode) (bool32ToBool triangleFans) (bool32ToBool vertexAttributeAccessBeyondStride)

instance Storable PhysicalDevicePortabilitySubsetFeaturesKHR where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePortabilitySubsetFeaturesKHR where
  zero = PhysicalDevicePortabilitySubsetFeaturesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDevicePortabilitySubsetPropertiesKHR - Structure describing
-- additional properties supported by a portable implementation
--
-- = Members
--
-- The members of the 'PhysicalDevicePortabilitySubsetPropertiesKHR'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDevicePortabilitySubsetPropertiesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePortabilitySubsetPropertiesKHR = PhysicalDevicePortabilitySubsetPropertiesKHR
  { -- | @minVertexInputBindingStrideAlignment@ indicates the minimum alignment
    -- for vertex input strides.
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@stride@ /must/
    -- be a multiple of, and at least as large as, this value.
    minVertexInputBindingStrideAlignment :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePortabilitySubsetPropertiesKHR)
#endif
deriving instance Show PhysicalDevicePortabilitySubsetPropertiesKHR

instance ToCStruct PhysicalDevicePortabilitySubsetPropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePortabilitySubsetPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minVertexInputBindingStrideAlignment)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDevicePortabilitySubsetPropertiesKHR where
  peekCStruct p = do
    minVertexInputBindingStrideAlignment <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDevicePortabilitySubsetPropertiesKHR
             minVertexInputBindingStrideAlignment

instance Storable PhysicalDevicePortabilitySubsetPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePortabilitySubsetPropertiesKHR where
  zero = PhysicalDevicePortabilitySubsetPropertiesKHR
           zero


type KHR_PORTABILITY_SUBSET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PORTABILITY_SUBSET_SPEC_VERSION"
pattern KHR_PORTABILITY_SUBSET_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PORTABILITY_SUBSET_SPEC_VERSION = 1


type KHR_PORTABILITY_SUBSET_EXTENSION_NAME = "VK_KHR_portability_subset"

-- No documentation found for TopLevel "VK_KHR_PORTABILITY_SUBSET_EXTENSION_NAME"
pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME = "VK_KHR_portability_subset"

