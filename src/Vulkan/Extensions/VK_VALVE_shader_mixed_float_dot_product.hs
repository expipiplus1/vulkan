{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_shader_mixed_float_dot_product - device extension
--
-- = VK_VALVE_shader_mixed_float_dot_product
--
-- [__Name String__]
--     @VK_VALVE_shader_mixed_float_dot_product@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     674
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float16_int8 VK_KHR_shader_float16_int8>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/VALVE/SPV_VALVE_mixed_float_dot_product.html SPV_VALVE_mixed_float_dot_product>
--
-- [__Contact__]
--
--     -   Georg Lehmann
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_shader_mixed_float_dot_product] @DadSchoorse%0A*Here describe the issue or question you have about the VK_VALVE_shader_mixed_float_dot_product extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-02-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Georg Lehmann, Valve
--
--     -   Mike Blumenkrantz, Valve
--
-- == Description
--
-- This extension enables support for mixed precision dot product
-- accumulate operations in shaders as defined in
-- @SPV_VALVE_mixed_float_dot_product@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME'
--
-- -   'VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MIXED_FLOAT_DOT_PRODUCT_FEATURES_VALVE'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductFloat16AccFloat32VALVE DotProductFloat16AccFloat32VALVE>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductFloat16AccFloat16VALVE DotProductFloat16AccFloat16VALVE>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductBFloat16AccVALVE DotProductBFloat16AccVALVE>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductFloat8AccFloat32VALVE DotProductFloat8AccFloat32VALVE>
--
-- == Version History
--
-- -   Revision 1, 2026-02-04 (Georg Lehmann)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_VALVE_shader_mixed_float_dot_product Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_shader_mixed_float_dot_product  ( PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE(..)
                                                                  , VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION
                                                                  , pattern VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION
                                                                  , VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME
                                                                  , pattern VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME
                                                                  ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MIXED_FLOAT_DOT_PRODUCT_FEATURES_VALVE))
-- | VkPhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE - Structure
-- describing support for mixed float dot products
--
-- = Description
--
-- If the 'PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE', it /must/ add
-- an instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_shader_mixed_float_dot_product VK_VALVE_shader_mixed_float_dot_product>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE = PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE
  { -- | #features-shaderMixedFloatDotProductFloat16AccFloat32#
    -- @shaderMixedFloatDotProductFloat16AccFloat32@ indicates whether the
    -- implementation supports shaders with the
    -- @DotProductFloat16AccFloat32VALVE@ capability.
    shaderMixedFloatDotProductFloat16AccFloat32 :: Bool
  , -- | #features-shaderMixedFloatDotProductFloat16AccFloat16#
    -- @shaderMixedFloatDotProductFloat16AccFloat16@ indicates whether the
    -- implementation supports shaders with the
    -- @DotProductFloat16AccFloat16VALVE@ capability.
    shaderMixedFloatDotProductFloat16AccFloat16 :: Bool
  , -- | #features-shaderMixedFloatDotProductBFloat16Acc#
    -- @shaderMixedFloatDotProductBFloat16Acc@ indicates whether the
    -- implementation supports shaders with the @DotProductBFloat16AccVALVE@
    -- capability.
    shaderMixedFloatDotProductBFloat16Acc :: Bool
  , -- | #features-shaderMixedFloatDotProductFloat8AccFloat32#
    -- @shaderMixedFloatDotProductFloat8AccFloat32@ indicates whether the
    -- implementation supports shaders with the
    -- @DotProductFloat8AccFloat32VALVE@ capability.
    shaderMixedFloatDotProductFloat8AccFloat32 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE)
#endif
deriving instance Show PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE

instance ToCStruct PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MIXED_FLOAT_DOT_PRODUCT_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderMixedFloatDotProductFloat16AccFloat32))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderMixedFloatDotProductFloat16AccFloat16))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderMixedFloatDotProductBFloat16Acc))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderMixedFloatDotProductFloat8AccFloat32))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MIXED_FLOAT_DOT_PRODUCT_FEATURES_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE where
  peekCStruct p = do
    shaderMixedFloatDotProductFloat16AccFloat32 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderMixedFloatDotProductFloat16AccFloat16 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderMixedFloatDotProductBFloat16Acc <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderMixedFloatDotProductFloat8AccFloat32 <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE
             (bool32ToBool shaderMixedFloatDotProductFloat16AccFloat32)
             (bool32ToBool shaderMixedFloatDotProductFloat16AccFloat16)
             (bool32ToBool shaderMixedFloatDotProductBFloat16Acc)
             (bool32ToBool shaderMixedFloatDotProductFloat8AccFloat32)

instance Storable PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE where
  zero = PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE
           zero
           zero
           zero
           zero


type VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION"
pattern VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION :: forall a . Integral a => a
pattern VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION = 1


type VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME = "VK_VALVE_shader_mixed_float_dot_product"

-- No documentation found for TopLevel "VK_VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME"
pattern VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME = "VK_VALVE_shader_mixed_float_dot_product"

