{-# language CPP #-}
-- | = Name
--
-- VK_ARM_shader_core_builtins - device extension
--
-- == VK_ARM_shader_core_builtins
--
-- [__Name String__]
--     @VK_ARM_shader_core_builtins@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     498
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_shader_core_builtins] @kevinpetit%0A*Here describe the issue or question you have about the VK_ARM_shader_core_builtins extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-10-05
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_core_builtins.html SPV_ARM_core_builtins>.
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/arm/GLSL_ARM_shader_core_builtins.txt GL_ARM_shader_core_builtins>
--
-- [__Contributors__]
--
--     -   Kevin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
-- == Description
--
-- This extension provides the ability to determine device-specific
-- properties on Arm GPUs. It exposes properties for the number of shader
-- cores, the maximum number of warps that can run on a shader core, and
-- shader builtins to enable invocations to identify which core and warp a
-- shader invocation is executing on.
--
-- This extension enables support for the SPIR-V @CoreBuiltinsARM@
-- capability.
--
-- These properties and built-ins can be used for debugging or performance
-- optimisation purposes. A typical optimisation example would be to use
-- @CoreIDARM@ to select a per-shader-core instance of a data structure in
-- algorithms that use atomics so as to reduce contention.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderCoreBuiltinsFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCoreBuiltinsPropertiesARM'
--
-- == New Enum Constants
--
-- -   'ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME'
--
-- -   'ARM_SHADER_CORE_BUILTINS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM'
--
-- == New or Modified Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-corecountarm CoreCountARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-coremaxidarm CoreMaxIDARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-coreidarm CoreIDARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-warpmaxidarm WarpsMaxIDARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-warpidarm WarpIDARM>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CoreBuiltinsARM CoreBuiltinsARM>
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-10-05 (Kevin Petit)
--
--     -   Initial revision
--
-- -   Revision 2, 2022-10-26 (Kevin Petit)
--
--     -   Add @shaderCoreMask@ property
--
-- == See Also
--
-- 'PhysicalDeviceShaderCoreBuiltinsFeaturesARM',
-- 'PhysicalDeviceShaderCoreBuiltinsPropertiesARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_shader_core_builtins Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_shader_core_builtins  ( PhysicalDeviceShaderCoreBuiltinsPropertiesARM(..)
                                                      , PhysicalDeviceShaderCoreBuiltinsFeaturesARM(..)
                                                      , ARM_SHADER_CORE_BUILTINS_SPEC_VERSION
                                                      , pattern ARM_SHADER_CORE_BUILTINS_SPEC_VERSION
                                                      , ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME
                                                      , pattern ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM))
-- | VkPhysicalDeviceShaderCoreBuiltinsPropertiesARM - Structure describing
-- shader core builtins properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderCoreBuiltinsPropertiesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_core_builtins VK_ARM_shader_core_builtins>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCoreBuiltinsPropertiesARM = PhysicalDeviceShaderCoreBuiltinsPropertiesARM
  { -- | #limits-shaderCoreMask# @shaderCoreMask@ is a bitfield where each bit
    -- set represents the presence of a shader core whose ID is the bit
    -- position. The highest ID for any shader core on the device is the
    -- position of the most significant bit set.
    shaderCoreMask :: Word64
  , -- | #limits-shaderCoreCount# @shaderCoreCount@ is the number of shader cores
    -- on the device.
    shaderCoreCount :: Word32
  , -- | #limits-shaderWarpsPerCore# @shaderWarpsPerCore@ is the maximum number
    -- of simultaneously executing warps on a shader core.
    shaderWarpsPerCore :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderCoreBuiltinsPropertiesARM)
#endif
deriving instance Show PhysicalDeviceShaderCoreBuiltinsPropertiesARM

instance ToCStruct PhysicalDeviceShaderCoreBuiltinsPropertiesARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderCoreBuiltinsPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (shaderCoreMask)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (shaderCoreCount)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderWarpsPerCore)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderCoreBuiltinsPropertiesARM where
  peekCStruct p = do
    shaderCoreMask <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    shaderCoreCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderWarpsPerCore <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceShaderCoreBuiltinsPropertiesARM
             shaderCoreMask shaderCoreCount shaderWarpsPerCore

instance Storable PhysicalDeviceShaderCoreBuiltinsPropertiesARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderCoreBuiltinsPropertiesARM where
  zero = PhysicalDeviceShaderCoreBuiltinsPropertiesARM
           zero
           zero
           zero


-- | VkPhysicalDeviceShaderCoreBuiltinsFeaturesARM - Structure describing the
-- shader core builtins features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderCoreBuiltinsFeaturesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderCoreBuiltinsFeaturesARM' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_core_builtins VK_ARM_shader_core_builtins>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCoreBuiltinsFeaturesARM = PhysicalDeviceShaderCoreBuiltinsFeaturesARM
  { -- | #features-shaderCoreBuiltins# @shaderCoreBuiltins@ indicates whether the
    -- implementation supports the SPIR-V @CoreBuiltinsARM@ capability.
    shaderCoreBuiltins :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderCoreBuiltinsFeaturesARM)
#endif
deriving instance Show PhysicalDeviceShaderCoreBuiltinsFeaturesARM

instance ToCStruct PhysicalDeviceShaderCoreBuiltinsFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderCoreBuiltinsFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderCoreBuiltins))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderCoreBuiltinsFeaturesARM where
  peekCStruct p = do
    shaderCoreBuiltins <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderCoreBuiltinsFeaturesARM
             (bool32ToBool shaderCoreBuiltins)

instance Storable PhysicalDeviceShaderCoreBuiltinsFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderCoreBuiltinsFeaturesARM where
  zero = PhysicalDeviceShaderCoreBuiltinsFeaturesARM
           zero


type ARM_SHADER_CORE_BUILTINS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_ARM_SHADER_CORE_BUILTINS_SPEC_VERSION"
pattern ARM_SHADER_CORE_BUILTINS_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_SHADER_CORE_BUILTINS_SPEC_VERSION = 2


type ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME = "VK_ARM_shader_core_builtins"

-- No documentation found for TopLevel "VK_ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME"
pattern ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME = "VK_ARM_shader_core_builtins"

