{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_64bit_indexing - device extension
--
-- = VK_EXT_shader_64bit_indexing
--
-- [__Name String__]
--     @VK_EXT_shader_64bit_indexing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     628
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_64bit_indexing.html SPV_EXT_shader_64bit_indexing>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_64bit_indexing] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_shader_64bit_indexing extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_64bit_indexing.adoc VK_EXT_shader_64bit_indexing>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-02
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GL_EXT_shader_64bit_indexing.txt GL_EXT_shader_64bit_indexing>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension relaxes the maxStorageBufferRange limit, allowing more
-- than 4GB to be accessed through a buffer binding (or through a buffer
-- device address). It adds pipeline and shader creation flags that request
-- 64-bit addressing support, and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-64bindexing defines>
-- which addressing calculations use 64 bits of range.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShader64BitIndexingFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME'
--
-- -   'EXT_SHADER_64BIT_INDEXING_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_64_BIT_INDEXING_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-05-02 (Jeff Bolz)
--
--     -   Initial revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_64bit_indexing Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_64bit_indexing  ( PhysicalDeviceShader64BitIndexingFeaturesEXT(..)
                                                       , EXT_SHADER_64BIT_INDEXING_SPEC_VERSION
                                                       , pattern EXT_SHADER_64BIT_INDEXING_SPEC_VERSION
                                                       , EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME
                                                       , pattern EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME
                                                       , ShaderCreateFlagBitsEXT(..)
                                                       , ShaderCreateFlagsEXT
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagsEXT)
-- | VkPhysicalDeviceShader64BitIndexingFeaturesEXT - Structure describing
-- 64-bit indexing features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShader64BitIndexingFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShader64BitIndexingFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_64bit_indexing VK_EXT_shader_64bit_indexing>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShader64BitIndexingFeaturesEXT = PhysicalDeviceShader64BitIndexingFeaturesEXT
  { -- | #features-shader64BitIndexing# @shader64BitIndexing@ indicates that the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-64bindexing using 64-bit address calculations>
    -- for indexing cooperative matrices, cooperative vectors, storage buffers,
    -- and physical storage buffers.
    shader64BitIndexing :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShader64BitIndexingFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShader64BitIndexingFeaturesEXT

instance ToCStruct PhysicalDeviceShader64BitIndexingFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShader64BitIndexingFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shader64BitIndexing))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShader64BitIndexingFeaturesEXT where
  peekCStruct p = do
    shader64BitIndexing <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShader64BitIndexingFeaturesEXT
             (bool32ToBool shader64BitIndexing)

instance Storable PhysicalDeviceShader64BitIndexingFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShader64BitIndexingFeaturesEXT where
  zero = PhysicalDeviceShader64BitIndexingFeaturesEXT
           zero


type EXT_SHADER_64BIT_INDEXING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_64BIT_INDEXING_SPEC_VERSION"
pattern EXT_SHADER_64BIT_INDEXING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_64BIT_INDEXING_SPEC_VERSION = 1


type EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME = "VK_EXT_shader_64bit_indexing"

-- No documentation found for TopLevel "VK_EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME"
pattern EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME = "VK_EXT_shader_64bit_indexing"

