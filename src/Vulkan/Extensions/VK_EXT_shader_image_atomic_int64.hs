{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_image_atomic_int64 - device extension
--
-- == VK_EXT_shader_image_atomic_int64
--
-- [__Name String__]
--     @VK_EXT_shader_image_atomic_int64@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     235
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_image_atomic_int64] @tobski%0A<<Here describe the issue or question you have about the VK_EXT_shader_image_atomic_int64 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Graham Wihlidal, Epic Games
--
--     -   Tobias Hector, AMD
--
--     -   Jeff Bolz, Nvidia
--
--     -   Jason Ekstrand, Intel
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires the
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_image_int64.html SPV_EXT_shader_image_int64>
--         SPIR-V extension.
--
--     -   This extension requires the
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_image_int64.txt GLSL_EXT_shader_image_int64>
--         extension for GLSL source languages.
--
-- == Description
--
-- This extension extends existing 64-bit integer atomic support to enable
-- these operations on images as well.
--
-- When working with large 2- or 3-dimensional data sets (e.g.
-- rasterization or screen-space effects), image accesses are generally
-- more efficient than equivalent buffer accesses. This extension allows
-- applications relying on 64-bit integer atomics in this manner to quickly
-- improve performance with only relatively minor code changes.
--
-- 64-bit integer atomic support is guaranteed for optimally tiled images
-- with the 'Vulkan.Core10.Enums.Format.FORMAT_R64_UINT' and
-- 'Vulkan.Core10.Enums.Format.FORMAT_R64_SINT' formats.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderImageAtomicInt64FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME'
--
-- -   'EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-07-14 (Tobias Hector)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceShaderImageAtomicInt64FeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_image_atomic_int64 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_image_atomic_int64  ( PhysicalDeviceShaderImageAtomicInt64FeaturesEXT(..)
                                                           , EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION
                                                           , pattern EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION
                                                           , EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
                                                           , pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT))
-- | VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT - Structure describing
-- features supported by VK_EXT_shader_image_atomic_int64
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceShaderAtomicInt64FeaturesEXT@ structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. @VkPhysicalDeviceShaderAtomicInt64FeaturesEXT@ /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_image_atomic_int64 VK_EXT_shader_image_atomic_int64>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
  { -- | #features-shaderImageInt64Atomics# @shaderImageInt64Atomics@ indicates
    -- whether shaders /can/ support 64-bit unsigned and signed integer atomic
    -- operations on images.
    shaderImageInt64Atomics :: Bool
  , -- | #features-sparseImageInt64Atomics# @sparseImageInt64Atomics@ indicates
    -- whether 64-bit integer atomics /can/ be used on sparse images.
    sparseImageInt64Atomics :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderImageAtomicInt64FeaturesEXT

instance ToCStruct PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderImageAtomicInt64FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderImageInt64Atomics))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sparseImageInt64Atomics))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  peekCStruct p = do
    shaderImageInt64Atomics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sparseImageInt64Atomics <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
             (bool32ToBool shaderImageInt64Atomics) (bool32ToBool sparseImageInt64Atomics)

instance Storable PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  zero = PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
           zero
           zero


type EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION"
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION = 1


type EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME = "VK_EXT_shader_image_atomic_int64"

-- No documentation found for TopLevel "VK_EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME"
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME = "VK_EXT_shader_image_atomic_int64"

