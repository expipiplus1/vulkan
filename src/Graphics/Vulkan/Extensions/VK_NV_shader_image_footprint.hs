{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint  ( PhysicalDeviceShaderImageFootprintFeaturesNV(..)
                                                                , NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
                                                                , pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
                                                                , NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
                                                                , pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV))
-- | VkPhysicalDeviceShaderImageFootprintFeaturesNV - Structure describing
-- shader image footprint features that can be supported by an
-- implementation
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-footprint Texel Footprint Evaluation>
-- for more information.
--
-- If the 'PhysicalDeviceShaderImageFootprintFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceShaderImageFootprintFeaturesNV' /can/ also be included in
-- the @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderImageFootprintFeaturesNV = PhysicalDeviceShaderImageFootprintFeaturesNV
  { -- | @imageFootprint@ specifies whether the implementation supports the
    -- @ImageFootprintNV@ SPIR-V capability.
    imageFootprint :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShaderImageFootprintFeaturesNV

instance ToCStruct PhysicalDeviceShaderImageFootprintFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderImageFootprintFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageFootprint))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderImageFootprintFeaturesNV where
  peekCStruct p = do
    imageFootprint <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderImageFootprintFeaturesNV
             (bool32ToBool imageFootprint)

instance Storable PhysicalDeviceShaderImageFootprintFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderImageFootprintFeaturesNV where
  zero = PhysicalDeviceShaderImageFootprintFeaturesNV
           zero


type NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION"
pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 2


type NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = "VK_NV_shader_image_footprint"

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME"
pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = "VK_NV_shader_image_footprint"

