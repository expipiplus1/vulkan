{-# language CPP #-}
-- | = Name
--
-- VK_ARM_shader_core_properties - device extension
--
-- == VK_ARM_shader_core_properties
--
-- [__Name String__]
--     @VK_ARM_shader_core_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     416
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_shader_core_properties] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_shader_core_properties extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
-- == Description
--
-- This extension provides the ability to determine device-specific
-- performance properties of Arm GPUs.
--
-- It exposes properties for the number of texel, pixel, and fused
-- multiply-add operations per clock per shader core. This can be used in
-- combination with the @VK_ARM_shader_core_builtins@ extension that
-- provides the ability to query the number of shader cores on the physical
-- device.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCorePropertiesARM'
--
-- == New Enum Constants
--
-- -   'ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME'
--
-- -   'ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM'
--
-- == Version History
--
-- -   Revision 1, 2023-02-07 (Jan-Harald Fredriksen)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceShaderCorePropertiesARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_shader_core_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_shader_core_properties  ( PhysicalDeviceShaderCorePropertiesARM(..)
                                                        , ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION
                                                        , pattern ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION
                                                        , ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME
                                                        , pattern ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM))
-- | VkPhysicalDeviceShaderCorePropertiesARM - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Description
--
-- If a throughput rate cannot be determined on the physical device, the
-- value @0@ will be returned for that rate.
--
-- If the 'PhysicalDeviceShaderCorePropertiesARM' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_core_properties VK_ARM_shader_core_properties>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCorePropertiesARM = PhysicalDeviceShaderCorePropertiesARM
  { -- | @pixelRate@ is an unsigned integer value indicating the maximum number
    -- of pixels output per clock per shader core.
    pixelRate :: Word32
  , -- | @texelRate@ is an unsigned integer value indicating the maximum number
    -- of texels per clock per shader core.
    texelRate :: Word32
  , -- | @fmaRate@ is an unsigned integer value indicating the maximum number of
    -- single-precision fused multiply-add operations per clock per shader
    -- core.
    fmaRate :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderCorePropertiesARM)
#endif
deriving instance Show PhysicalDeviceShaderCorePropertiesARM

instance ToCStruct PhysicalDeviceShaderCorePropertiesARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderCorePropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (pixelRate)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (texelRate)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (fmaRate)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderCorePropertiesARM where
  peekCStruct p = do
    pixelRate <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    texelRate <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    fmaRate <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PhysicalDeviceShaderCorePropertiesARM
             pixelRate texelRate fmaRate

instance Storable PhysicalDeviceShaderCorePropertiesARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderCorePropertiesARM where
  zero = PhysicalDeviceShaderCorePropertiesARM
           zero
           zero
           zero


type ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION"
pattern ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION = 1


type ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME = "VK_ARM_shader_core_properties"

-- No documentation found for TopLevel "VK_ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME"
pattern ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME = "VK_ARM_shader_core_properties"

