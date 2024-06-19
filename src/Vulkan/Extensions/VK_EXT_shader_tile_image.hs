{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_tile_image - device extension
--
-- == VK_EXT_shader_tile_image
--
-- [__Name String__]
--     @VK_EXT_shader_tile_image@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     396
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_tile_image.html SPV_EXT_shader_tile_image>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_tile_image] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_shader_tile_image extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_tile_image.adoc VK_EXT_shader_tile_image>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://raw.githubusercontent.com/KhronosGroup/GLSL/master/extensions/ext/GLSL_EXT_shader_tile_image.txt GL_EXT_shader_tile_image>
--
-- [__Contributors__]
--
--     -   Sandeep Kakarlapudi, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   James Fitzpatrick, Imagination
--
--     -   Andrew Garrard, Imagination
--
--     -   Jeff Leger, Qualcomm
--
--     -   Huilong Wang, Huawei
--
--     -   Graeme Leese, Broadcom
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Tobias Hector, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Shahbaz Youssefi, Google
--
-- == Description
--
-- This extension allows fragment shader invocations to read color, depth
-- and stencil values at their pixel location in rasterization order. The
-- functionality is only available when using dynamic render passes
-- introduced by VK_KHR_dynamic_rendering. Example use cases are
-- programmable blending and deferred shading.
--
-- See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-shader-tileimage-reads fragment shader tile image reads>
-- for more information.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderTileImageFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderTileImagePropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_TILE_IMAGE_EXTENSION_NAME'
--
-- -   'EXT_SHADER_TILE_IMAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- Color read example.
--
-- > layout( location = 0 /* aliased to color attachment 0 */ ) tileImageEXT highp attachmentEXT color0;
-- > layout( location = 1 /* aliased to color attachment 1 */ ) tileImageEXT highp attachmentEXT color1;
-- >
-- > layout( location = 0 ) out vec4 fragColor;
-- >
-- > void main()
-- > {
-- >     vec4 value = colorAttachmentReadEXT(color0) + colorAttachmentReadEXT(color1);
-- >     fragColor = value;
-- > }
--
-- Depth & Stencil read example.
--
-- > void main()
-- > {
-- >     // read sample 0: works for non-MSAA or MSAA targets
-- >     highp float last_depth = depthAttachmentReadEXT();
-- >     lowp uint last_stencil = stencilAttachmentReadEXT();
-- >
-- >     //..
-- > }
--
-- == Version History
--
-- -   Revision 1, 2023-03-23 (Sandeep Kakarlapudi)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceShaderTileImageFeaturesEXT',
-- 'PhysicalDeviceShaderTileImagePropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_tile_image Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_tile_image  ( PhysicalDeviceShaderTileImageFeaturesEXT(..)
                                                   , PhysicalDeviceShaderTileImagePropertiesEXT(..)
                                                   , EXT_SHADER_TILE_IMAGE_SPEC_VERSION
                                                   , pattern EXT_SHADER_TILE_IMAGE_SPEC_VERSION
                                                   , EXT_SHADER_TILE_IMAGE_EXTENSION_NAME
                                                   , pattern EXT_SHADER_TILE_IMAGE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT))
-- | VkPhysicalDeviceShaderTileImageFeaturesEXT - Structure describing tile
-- image features supported by the implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderTileImageFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderTileImageFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderTileImageFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_tile_image VK_EXT_shader_tile_image>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderTileImageFeaturesEXT = PhysicalDeviceShaderTileImageFeaturesEXT
  { -- | #features-shaderTileImageColorReadAccess#
    -- @shaderTileImageColorReadAccess@ indicates that the implementation
    -- supports the @TileImageColorReadAccessEXT@ SPIR-V capability.
    shaderTileImageColorReadAccess :: Bool
  , -- | #features-shaderTileImageDepthReadAccess#
    -- @shaderTileImageDepthReadAccess@ indicates that the implementation
    -- supports the @TileImageDepthReadAccessEXT@ SPIR-V capability.
    shaderTileImageDepthReadAccess :: Bool
  , -- | #features-shaderTileImageStencilReadAccess#
    -- @shaderTileImageStencilReadAccess@ indicates that the implementation
    -- supports the @TileImageStencilReadAccessEXT@ SPIR-V capability.
    shaderTileImageStencilReadAccess :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderTileImageFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderTileImageFeaturesEXT

instance ToCStruct PhysicalDeviceShaderTileImageFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderTileImageFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderTileImageColorReadAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderTileImageDepthReadAccess))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderTileImageStencilReadAccess))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderTileImageFeaturesEXT where
  peekCStruct p = do
    shaderTileImageColorReadAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderTileImageDepthReadAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderTileImageStencilReadAccess <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderTileImageFeaturesEXT
             (bool32ToBool shaderTileImageColorReadAccess)
             (bool32ToBool shaderTileImageDepthReadAccess)
             (bool32ToBool shaderTileImageStencilReadAccess)

instance Storable PhysicalDeviceShaderTileImageFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderTileImageFeaturesEXT where
  zero = PhysicalDeviceShaderTileImageFeaturesEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceShaderTileImagePropertiesEXT - Structure containing
-- information about tile image support for a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceShaderTileImagePropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These are properties of the tile image information of a physical device.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_tile_image VK_EXT_shader_tile_image>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderTileImagePropertiesEXT = PhysicalDeviceShaderTileImagePropertiesEXT
  { -- | @shaderTileImageCoherentReadAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if coherent reads of tile image
    -- data is accelerated.
    shaderTileImageCoherentReadAccelerated :: Bool
  , -- | @shaderTileImageReadSampleFromPixelRateInvocation@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if reading from samples
    -- from a pixel rate fragment invocation is supported when
    -- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
    -- > 1.
    shaderTileImageReadSampleFromPixelRateInvocation :: Bool
  , -- | @shaderTileImageReadFromHelperInvocation@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if reads of tile image data from
    -- helper fragment invocations result in valid values.
    shaderTileImageReadFromHelperInvocation :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderTileImagePropertiesEXT)
#endif
deriving instance Show PhysicalDeviceShaderTileImagePropertiesEXT

instance ToCStruct PhysicalDeviceShaderTileImagePropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderTileImagePropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderTileImageCoherentReadAccelerated))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderTileImageReadSampleFromPixelRateInvocation))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderTileImageReadFromHelperInvocation))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderTileImagePropertiesEXT where
  peekCStruct p = do
    shaderTileImageCoherentReadAccelerated <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderTileImageReadSampleFromPixelRateInvocation <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderTileImageReadFromHelperInvocation <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderTileImagePropertiesEXT
             (bool32ToBool shaderTileImageCoherentReadAccelerated)
             (bool32ToBool shaderTileImageReadSampleFromPixelRateInvocation)
             (bool32ToBool shaderTileImageReadFromHelperInvocation)

instance Storable PhysicalDeviceShaderTileImagePropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderTileImagePropertiesEXT where
  zero = PhysicalDeviceShaderTileImagePropertiesEXT
           zero
           zero
           zero


type EXT_SHADER_TILE_IMAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_TILE_IMAGE_SPEC_VERSION"
pattern EXT_SHADER_TILE_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_TILE_IMAGE_SPEC_VERSION = 1


type EXT_SHADER_TILE_IMAGE_EXTENSION_NAME = "VK_EXT_shader_tile_image"

-- No documentation found for TopLevel "VK_EXT_SHADER_TILE_IMAGE_EXTENSION_NAME"
pattern EXT_SHADER_TILE_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_TILE_IMAGE_EXTENSION_NAME = "VK_EXT_shader_tile_image"

