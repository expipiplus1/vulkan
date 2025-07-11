{-# language CPP #-}
-- | = Name
--
-- VK_NV_shader_image_footprint - device extension
--
-- == VK_NV_shader_image_footprint
--
-- [__Name String__]
--     @VK_NV_shader_image_footprint@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     205
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_image_footprint.html SPV_NV_shader_image_footprint>
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_shader_image_footprint] @nvpbrown%0A*Here describe the issue or question you have about the VK_NV_shader_image_footprint extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_shader_texture_footprint.txt GL_NV_shader_texture_footprint>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Chris Lentini, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_image_footprint.html SPV_NV_shader_image_footprint>
-- SPIR-V extension. That SPIR-V extension provides a new instruction
-- @OpImageSampleFootprintNV@ allowing shaders to determine the set of
-- texels that would be accessed by an equivalent filtered texture lookup.
--
-- Instead of returning a filtered texture value, the instruction returns a
-- structure that can be interpreted by shader code to determine the
-- footprint of a filtered texture lookup. This structure includes integer
-- values that identify a small neighborhood of texels in the image being
-- accessed and a bitfield that indicates which texels in that neighborhood
-- would be used. The structure also includes a bitfield where each bit
-- identifies whether any texel in a small aligned block of texels would be
-- fetched by the texture lookup. The size of each block is specified by an
-- access /granularity/ provided by the shader. The minimum granularity
-- supported by this extension is 2x2 (for 2D textures) and 2x2x2 (for 3D
-- textures); the maximum granularity is 256x256 (for 2D textures) or
-- 64x32x32 (for 3D textures). Each footprint query returns the footprint
-- from a single texture level. When using minification filters that
-- combine accesses from multiple mipmap levels, shaders must perform
-- separate queries for the two levels accessed (“fine” and “coarse”). The
-- footprint query also returns a flag indicating if the texture lookup
-- would access texels from only one mipmap level or from two neighboring
-- levels.
--
-- This extension should be useful for multi-pass rendering operations that
-- do an initial expensive rendering pass to produce a first image that is
-- then used as a texture for a second pass. If the second pass ends up
-- accessing only portions of the first image (e.g., due to visibility),
-- the work spent rendering the non-accessed portion of the first image was
-- wasted. With this feature, an application can limit this waste using an
-- initial pass over the geometry in the second image that performs a
-- footprint query for each visible pixel to determine the set of pixels
-- that it needs from the first image. This pass would accumulate an
-- aggregate footprint of all visible pixels into a separate “footprint
-- image” using shader atomics. Then, when rendering the first image, the
-- application can kill all shading work for pixels not in this aggregate
-- footprint.
--
-- This extension has a number of limitations. The
-- @OpImageSampleFootprintNV@ instruction only supports for two- and
-- three-dimensional textures. Footprint evaluation only supports the
-- CLAMP_TO_EDGE wrap mode; results are undefined for all other wrap modes.
-- Only a limited set of granularity values and that set does not support
-- separate coverage information for each texel in the original image.
--
-- When using SPIR-V generated from the OpenGL Shading Language, the new
-- instruction will be generated from code using the new
-- @textureFootprint*NV@ built-in functions from the
-- @GL_NV_shader_texture_footprint@ shading language extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderImageFootprintFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME'
--
-- -   'NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV'
--
-- == New SPIR-V Capability
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ImageFootprintNV ImageFootprintNV>
--
-- == Issues
--
-- (1) The footprint returned by the SPIR-V instruction is a structure that
-- includes an anchor, an offset, and a mask that represents a 8x8 or 4x4x4
-- neighborhood of texel groups. But the bits of the mask are not stored in
-- simple pitch order. Why is the footprint built this way?
--
-- __RESOLVED__: We expect that applications using this feature will want
-- to use a fixed granularity and accumulate coverage information from the
-- returned footprints into an aggregate “footprint image” that tracks the
-- portions of an image that would be needed by regular texture filtering.
-- If an application is using a two-dimensional image with 4x4 pixel
-- granularity, we expect that the footprint image will use 64-bit texels
-- where each bit in an 8x8 array of bits corresponds to coverage for a 4x4
-- block in the original image. Texel (0,0) in the footprint image would
-- correspond to texels (0,0) through (31,31) in the original image.
--
-- In the usual case, the footprint for a single access will fully
-- contained in a 32x32 aligned region of the original texture, which
-- corresponds to a single 64-bit texel in the footprint image. In that
-- case, the implementation will return an anchor coordinate pointing at
-- the single footprint image texel, an offset vector of (0,0), and a mask
-- whose bits are aligned with the bits in the footprint texel. For this
-- case, the shader can simply atomically OR the mask bits into the
-- contents of the footprint texel to accumulate footprint coverage.
--
-- In the worst case, the footprint for a single access spans multiple
-- 32x32 aligned regions and may require updates to four separate footprint
-- image texels. In this case, the implementation will return an anchor
-- coordinate pointing at the lower right footprint image texel and an
-- offset will identify how many “columns” and “rows” of the returned 8x8
-- mask correspond to footprint texels to the left and above the anchor
-- texel. If the anchor is (2,3), the 64 bits of the returned mask are
-- arranged spatially as follows, where each 4x4 block is assigned a bit
-- number that matches its bit number in the footprint image texels:
--
-- >     +-------------------------+-------------------------+
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- 46 47 | 40 41 42 43 44 45 -- -- |
-- >     | -- -- -- -- -- -- 54 55 | 48 49 50 51 52 53 -- -- |
-- >     | -- -- -- -- -- -- 62 63 | 56 57 58 59 60 61 -- -- |
-- >     +-------------------------+-------------------------+
-- >     | -- -- -- -- -- -- 06 07 | 00 01 02 03 04 05 -- -- |
-- >     | -- -- -- -- -- -- 14 15 | 08 09 10 11 12 13 -- -- |
-- >     | -- -- -- -- -- -- 22 23 | 16 17 18 19 20 21 -- -- |
-- >     | -- -- -- -- -- -- 30 31 | 24 25 26 27 28 29 -- -- |
-- >     | -- -- -- -- -- -- 38 39 | 32 33 34 35 36 37 -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     | -- -- -- -- -- -- -- -- | -- -- -- -- -- -- -- -- |
-- >     +-------------------------+-------------------------+
--
-- To accumulate coverage for each of the four footprint image texels, a
-- shader can AND the returned mask with simple masks derived from the x
-- and y offset values and then atomically OR the updated mask bits into
-- the contents of the corresponding footprint texel.
--
-- >     uint64_t returnedMask = (uint64_t(footprint.mask.x) | (uint64_t(footprint.mask.y) << 32));
-- >     uint64_t rightMask    = ((0xFF >> footprint.offset.x) * 0x0101010101010101UL);
-- >     uint64_t bottomMask   = 0xFFFFFFFFFFFFFFFFUL >> (8 * footprint.offset.y);
-- >     uint64_t bottomRight  = returnedMask & bottomMask & rightMask;
-- >     uint64_t bottomLeft   = returnedMask & bottomMask & (~rightMask);
-- >     uint64_t topRight     = returnedMask & (~bottomMask) & rightMask;
-- >     uint64_t topLeft      = returnedMask & (~bottomMask) & (~rightMask);
--
-- (2) What should an application do to ensure maximum performance when
-- accumulating footprints into an aggregate footprint image?
--
-- __RESOLVED__: We expect that the most common usage of this feature will
-- be to accumulate aggregate footprint coverage, as described in the
-- previous issue. Even if you ignore the anisotropic filtering case where
-- the implementation may return a granularity larger than that requested
-- by the caller, each shader invocation will need to use atomic functions
-- to update up to four footprint image texels for each LOD accessed.
-- Having each active shader invocation perform multiple atomic operations
-- can be expensive, particularly when neighboring invocations will want to
-- update the same footprint image texels.
--
-- Techniques can be used to reduce the number of atomic operations
-- performed when accumulating coverage include:
--
-- -   Have logic that detects returned footprints where all components of
--     the returned offset vector are zero. In that case, the mask returned
--     by the footprint function is guaranteed to be aligned with the
--     footprint image texels and affects only a single footprint image
--     texel.
--
-- -   Have fragment shaders communicate using built-in functions from the
--     @VK_NV_shader_subgroup_partitioned@ extension or other shader
--     subgroup extensions. If you have multiple invocations in a subgroup
--     that need to update the same texel (x,y) in the footprint image,
--     compute an aggregate footprint mask across all invocations in the
--     subgroup updating that texel and have a single invocation perform an
--     atomic operation using that aggregate mask.
--
-- -   When the returned footprint spans multiple texels in the footprint
--     image, each invocation need to perform four atomic operations. In
--     the previous issue, we had an example that computed separate masks
--     for “topLeft”, “topRight”, “bottomLeft”, and “bottomRight”. When the
--     invocations in a subgroup have good locality, it might be the case
--     the “top left” for some invocations might refer to footprint image
--     texel (10,10), while neighbors might have their “top left” texels at
--     (11,10), (10,11), and (11,11). If you compute separate masks for
--     even\/odd x and y values instead of left\/right or top\/bottom, the
--     “odd\/odd” mask for all invocations in the subgroup hold coverage
--     for footprint image texel (11,11), which can be updated by a single
--     atomic operation for the entire subgroup.
--
-- == Examples
--
-- TBD
--
-- == Version History
--
-- -   Revision 2, 2018-09-13 (Pat Brown)
--
--     -   Add issue (2) with performance tips.
--
-- -   Revision 1, 2018-08-12 (Pat Brown)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceShaderImageFootprintFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_shader_image_footprint Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shader_image_footprint  ( PhysicalDeviceShaderImageFootprintFeaturesNV(..)
                                                       , NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
                                                       , pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
                                                       , NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
                                                       , pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV))
-- | VkPhysicalDeviceShaderImageFootprintFeaturesNV - Structure describing
-- shader image footprint features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-footprint Texel Footprint Evaluation>
-- for more information.
--
-- If the 'PhysicalDeviceShaderImageFootprintFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderImageFootprintFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shader_image_footprint VK_NV_shader_image_footprint>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderImageFootprintFeaturesNV = PhysicalDeviceShaderImageFootprintFeaturesNV
  { -- | #features-imageFootprint# @imageFootprint@ specifies whether the
    -- implementation supports the @ImageFootprintNV@ SPIR-V capability.
    imageFootprint :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderImageFootprintFeaturesNV)
#endif
deriving instance Show PhysicalDeviceShaderImageFootprintFeaturesNV

instance ToCStruct PhysicalDeviceShaderImageFootprintFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

