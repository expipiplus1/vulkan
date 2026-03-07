{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_fma - device extension
--
-- = VK_KHR_shader_fma
--
-- [__Name String__]
--     @VK_KHR_shader_fma@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     580
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fma.html SPV_KHR_fma>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_fma] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_shader_fma extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_fma.adoc VK_KHR_shader_fma>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-10
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension allows applications to use the SPV_KHR_fma extension to
-- obtain correctly-rounded results for fused-multiply add (fma)
-- operations.
--
-- Fused-multiply add is a building block of many high-precision numerical
-- functions. It provides better accuracy than separate operations, because
-- of the removal of the intermediate rounding step, and often costs less
-- than the pair of separate operations.
--
-- Vulkan currently exposes an fma primitive that can give the reduced
-- cost, but it is not guaranteed to be a fused operation, so the accuracy
-- cannot be relied on. For applications which require the high accuracy,
-- therefore, the operation must be emulated or the algorithm changed so as
-- not to require fma. This is often vastly more costly, even though fma is
-- supported in much of the underlying hardware.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderFmaFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_FMA_EXTENSION_NAME'
--
-- -   'KHR_SHADER_FMA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-06-10 (Graeme Leese)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_fma Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_fma  ( PhysicalDeviceShaderFmaFeaturesKHR(..)
                                            , KHR_SHADER_FMA_SPEC_VERSION
                                            , pattern KHR_SHADER_FMA_SPEC_VERSION
                                            , KHR_SHADER_FMA_EXTENSION_NAME
                                            , pattern KHR_SHADER_FMA_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR))
-- | VkPhysicalDeviceShaderFmaFeaturesKHR - Structure indicating support for
-- SPV_KHR_fma OpFmaKHR
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderFmaFeaturesKHR' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderFmaFeaturesKHR', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_fma VK_KHR_shader_fma>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderFmaFeaturesKHR = PhysicalDeviceShaderFmaFeaturesKHR
  { -- | #features-shaderFmaFloat16# @shaderFmaFloat16@ indicates whether the
    -- implementation supports @OpFmaKHR@ for Float16 types in shaders.
    shaderFmaFloat16 :: Bool
  , -- | #features-shaderFmaFloat32# @shaderFmaFloat32@ indicates whether the
    -- implementation supports @OpFmaKHR@ for Float32 types in shaders.
    shaderFmaFloat32 :: Bool
  , -- | #features-shaderFmaFloat64# @shaderFmaFloat64@ indicates whether the
    -- implementation supports @OpFmaKHR@ for Float64 types in shaders.
    shaderFmaFloat64 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderFmaFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderFmaFeaturesKHR

instance ToCStruct PhysicalDeviceShaderFmaFeaturesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderFmaFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderFmaFloat16))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderFmaFloat32))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderFmaFloat64))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderFmaFeaturesKHR where
  peekCStruct p = do
    shaderFmaFloat16 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderFmaFloat32 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderFmaFloat64 <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderFmaFeaturesKHR
             (bool32ToBool shaderFmaFloat16)
             (bool32ToBool shaderFmaFloat32)
             (bool32ToBool shaderFmaFloat64)

instance Storable PhysicalDeviceShaderFmaFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderFmaFeaturesKHR where
  zero = PhysicalDeviceShaderFmaFeaturesKHR
           zero
           zero
           zero


type KHR_SHADER_FMA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_FMA_SPEC_VERSION"
pattern KHR_SHADER_FMA_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_FMA_SPEC_VERSION = 1


type KHR_SHADER_FMA_EXTENSION_NAME = "VK_KHR_shader_fma"

-- No documentation found for TopLevel "VK_KHR_SHADER_FMA_EXTENSION_NAME"
pattern KHR_SHADER_FMA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_FMA_EXTENSION_NAME = "VK_KHR_shader_fma"

