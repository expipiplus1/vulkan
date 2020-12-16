{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_clock - device extension
--
-- == VK_KHR_shader_clock
--
-- [__Name String__]
--     @VK_KHR_shader_clock@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     182
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
--     -   Aaron Hagan
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_shader_clock:%20&body=@ahagan%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-4-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_shader_clock.html SPV_KHR_shader_clock>.
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_clock.txt ARB_shader_clock>
--         and
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shader_realtime_clock.txt EXT_shader_realtime_clock>
--
-- [__Contributors__]
--
--     -   Aaron Hagan, AMD
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension advertises the SPIR-V @ShaderClockKHR@ capability for
-- Vulkan, which allows a shader to query a real-time or monotonically
-- incrementing counter at the subgroup level or across the device level.
-- The two valid SPIR-V scopes for @OpReadClockKHR@ are @Subgroup@ and
-- 'Vulkan.Core10.Handles.Device'.
--
-- When using GLSL source-based shading languages, the
-- @clockRealtime@*@EXT@() timing functions map to the @OpReadClockKHR@
-- instruction with a scope of 'Vulkan.Core10.Handles.Device', and the
-- @clock@*@ARB@() timing functions map to the @OpReadClockKHR@ instruction
-- with a scope of @Subgroup@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderClockFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_CLOCK_EXTENSION_NAME'
--
-- -   'KHR_SHADER_CLOCK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderClockKHR ShaderClockKHR>
--
-- == Version History
--
-- -   Revision 1, 2019-4-25 (Aaron Hagan)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceShaderClockFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_clock Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_clock  ( PhysicalDeviceShaderClockFeaturesKHR(..)
                                              , KHR_SHADER_CLOCK_SPEC_VERSION
                                              , pattern KHR_SHADER_CLOCK_SPEC_VERSION
                                              , KHR_SHADER_CLOCK_EXTENSION_NAME
                                              , pattern KHR_SHADER_CLOCK_EXTENSION_NAME
                                              ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR))
-- | VkPhysicalDeviceShaderClockFeaturesKHR - Structure describing features
-- supported by VK_KHR_shader_clock
--
-- = Description
--
-- If the 'PhysicalDeviceShaderClockFeaturesKHR' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceShaderClockFeaturesKHR' can also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderClockFeaturesKHR = PhysicalDeviceShaderClockFeaturesKHR
  { -- | #features-shaderSubgroupClock# @shaderSubgroupClock@ indicates whether
    -- shaders /can/ perform @Subgroup@ scoped clock reads.
    shaderSubgroupClock :: Bool
  , -- | #features-shaderDeviceClock# @shaderDeviceClock@ indicates whether
    -- shaders /can/ perform 'Vulkan.Core10.Handles.Device' scoped clock reads.
    shaderDeviceClock :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderClockFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderClockFeaturesKHR

instance ToCStruct PhysicalDeviceShaderClockFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderClockFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupClock))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderDeviceClock))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderClockFeaturesKHR where
  peekCStruct p = do
    shaderSubgroupClock <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderDeviceClock <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderClockFeaturesKHR
             (bool32ToBool shaderSubgroupClock) (bool32ToBool shaderDeviceClock)

instance Storable PhysicalDeviceShaderClockFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderClockFeaturesKHR where
  zero = PhysicalDeviceShaderClockFeaturesKHR
           zero
           zero


type KHR_SHADER_CLOCK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_CLOCK_SPEC_VERSION"
pattern KHR_SHADER_CLOCK_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_CLOCK_SPEC_VERSION = 1


type KHR_SHADER_CLOCK_EXTENSION_NAME = "VK_KHR_shader_clock"

-- No documentation found for TopLevel "VK_KHR_SHADER_CLOCK_EXTENSION_NAME"
pattern KHR_SHADER_CLOCK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_CLOCK_EXTENSION_NAME = "VK_KHR_shader_clock"

