{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_portability_subset"
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

-- No documentation found for TopLevel "VkPhysicalDevicePortabilitySubsetFeaturesKHR"
data PhysicalDevicePortabilitySubsetFeaturesKHR = PhysicalDevicePortabilitySubsetFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "constantAlphaColorBlendFactors"
    constantAlphaColorBlendFactors :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "events"
    events :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "imageViewFormatReinterpretation"
    imageViewFormatReinterpretation :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "imageViewFormatSwizzle"
    imageViewFormatSwizzle :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "imageView2DOn3DImage"
    imageView2DOn3DImage :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "multisampleArrayImage"
    multisampleArrayImage :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "mutableComparisonSamplers"
    mutableComparisonSamplers :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "pointPolygons"
    pointPolygons :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "samplerMipLodBias"
    samplerMipLodBias :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "separateStencilMaskRef"
    separateStencilMaskRef :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "shaderSampleRateInterpolationFunctions"
    shaderSampleRateInterpolationFunctions :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "tessellationIsolines"
    tessellationIsolines :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "tessellationPointMode"
    tessellationPointMode :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "triangleFans"
    triangleFans :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetFeaturesKHR" "vertexAttributeAccessBeyondStride"
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



-- No documentation found for TopLevel "VkPhysicalDevicePortabilitySubsetPropertiesKHR"
data PhysicalDevicePortabilitySubsetPropertiesKHR = PhysicalDevicePortabilitySubsetPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePortabilitySubsetPropertiesKHR" "minVertexInputBindingStrideAlignment"
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

