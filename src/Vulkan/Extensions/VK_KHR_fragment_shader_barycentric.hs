{-# language CPP #-}
-- | = Name
--
-- VK_KHR_fragment_shader_barycentric - device extension
--
-- == VK_KHR_fragment_shader_barycentric
--
-- [__Name String__]
--     @VK_KHR_fragment_shader_barycentric@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     323
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Stu Smith
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_fragment_shader_barycentric.adoc VK_KHR_fragment_shader_barycentric>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fragment_shader_barycentric.html SPV_KHR_fragment_shader_barycentric>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_fragment_shader_barycentric.txt GL_EXT_fragment_shader_barycentric>
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Slawek Grajewski, Intel
--
--     -   Pat Brown, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Contributors to the VK_NV_fragment_shader_barycentric
--         specification
--
-- == Description
--
-- This extension is based on the @VK_NV_fragment_shader_barycentric@
-- extension, and adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fragment_shader_barycentric.html SPV_KHR_fragment_shader_barycentric>
--
-- The extension provides access to three additional fragment shader
-- variable decorations in SPIR-V:
--
-- -   @PerVertexKHR@, which indicates that a fragment shader input will
--     not have interpolated values, but instead must be accessed with an
--     extra array index that identifies one of the vertices of the
--     primitive producing the fragment
--
-- -   @BaryCoordKHR@, which indicates that the variable is a
--     three-component floating-point vector holding barycentric weights
--     for the fragment produced using perspective interpolation
--
-- -   @BaryCoordNoPerspKHR@, which indicates that the variable is a
--     three-component floating-point vector holding barycentric weights
--     for the fragment produced using linear interpolation
--
-- When using GLSL source-based shader languages, the following variables
-- from @GL_EXT_fragment_shader_barycentric@ map to these SPIR-V built-in
-- decorations:
--
-- -   @in vec3 gl_BaryCoordEXT;@ → @BaryCoordKHR@
--
-- -   @in vec3 gl_BaryCoordNoPerspEXT;@ → @BaryCoordNoPerspKHR@
--
-- GLSL variables declared using the @pervertexEXT@ GLSL qualifier are
-- expected to be decorated with @PerVertexKHR@ in SPIR-V.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShaderBarycentricFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentShaderBarycentricPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME'
--
-- -   'KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR'
--
-- == New Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-barycoordkhr BaryCoordKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-barycoordnoperspkhr BaryCoordNoPerspKHR>
--
-- == New SPIR-V Decorations
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-interpolation-decorations-pervertexkhr PerVertexKHR>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentBarycentricKHR FragmentBarycentricKHR>
--
-- == Issues
--
-- 1) What are the interactions with MSAA and how are @BaryCoordKHR@ and
-- @BaryCoordNoPerspKHR@ interpolated?
--
-- __RESOLVED__: The inputs decorated with @BaryCoordKHR@ or
-- @BaryCoordNoPerspKHR@ /may/ also be decorated with the @Centroid@ or
-- @Sample@ qualifiers to specify interpolation, like any other fragment
-- shader input. If
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderSampleRateInterpolationFunctions shaderSampleRateInterpolationFunctions>
-- is enabled, the extended instructions InterpolateAtCentroid,
-- InterpolateAtOffset, and InterpolateAtSample from the GLSL.std.450 /may/
-- also be used with inputs decorated with @BaryCoordKHR@ or
-- @BaryCoordNoPerspKHR@.
--
-- == Version History
--
-- -   Revision 1, 2022-03-10 (Stu Smith)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceFragmentShaderBarycentricFeaturesKHR',
-- 'PhysicalDeviceFragmentShaderBarycentricPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_fragment_shader_barycentric Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_fragment_shader_barycentric  ( PhysicalDeviceFragmentShaderBarycentricFeaturesKHR(..)
                                                             , PhysicalDeviceFragmentShaderBarycentricPropertiesKHR(..)
                                                             , KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
                                                             , pattern KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
                                                             , KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
                                                             , pattern KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR))
-- | VkPhysicalDeviceFragmentShaderBarycentricFeaturesKHR - Structure
-- describing barycentric support in fragment shaders that can be supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-barycentric Barycentric Interpolation>
-- for more information.
--
-- If the 'PhysicalDeviceFragmentShaderBarycentricFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceFragmentShaderBarycentricFeaturesKHR' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shader_barycentric VK_KHR_fragment_shader_barycentric>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShaderBarycentricFeaturesKHR = PhysicalDeviceFragmentShaderBarycentricFeaturesKHR
  { -- | #features-fragmentShaderBarycentric# @fragmentShaderBarycentric@
    -- indicates that the implementation supports the @BaryCoordKHR@ and
    -- @BaryCoordNoPerspKHR@ SPIR-V fragment shader built-ins and supports the
    -- @PerVertexKHR@ SPIR-V decoration on fragment shader input variables.
    fragmentShaderBarycentric :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShaderBarycentricFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShaderBarycentricFeaturesKHR

instance ToCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShaderBarycentricFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentShaderBarycentric))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesKHR where
  peekCStruct p = do
    fragmentShaderBarycentric <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShaderBarycentricFeaturesKHR
             (bool32ToBool fragmentShaderBarycentric)

instance Storable PhysicalDeviceFragmentShaderBarycentricFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShaderBarycentricFeaturesKHR where
  zero = PhysicalDeviceFragmentShaderBarycentricFeaturesKHR
           zero


-- | VkPhysicalDeviceFragmentShaderBarycentricPropertiesKHR - Structure
-- describing fragment shader barycentric limits of an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShaderBarycentricPropertiesKHR' structure
-- is included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shader_barycentric VK_KHR_fragment_shader_barycentric>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShaderBarycentricPropertiesKHR = PhysicalDeviceFragmentShaderBarycentricPropertiesKHR
  { -- | #limits-triStripVertexOrderIndependentOfProvokingVertex#
    -- @triStripVertexOrderIndependentOfProvokingVertex@ indicates that the
    -- implementation does not change its vertex numbering for triangle strip
    -- primitives when the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#vertexpostproc-flatshading provoking vertex mode>
    -- is
    -- 'Vulkan.Extensions.VK_EXT_provoking_vertex.PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT',
    -- as shown in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-barycentric-order-table-last-vertex last vertex table>.
    triStripVertexOrderIndependentOfProvokingVertex :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShaderBarycentricPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShaderBarycentricPropertiesKHR

instance ToCStruct PhysicalDeviceFragmentShaderBarycentricPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShaderBarycentricPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (triStripVertexOrderIndependentOfProvokingVertex))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShaderBarycentricPropertiesKHR where
  peekCStruct p = do
    triStripVertexOrderIndependentOfProvokingVertex <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShaderBarycentricPropertiesKHR
             (bool32ToBool triStripVertexOrderIndependentOfProvokingVertex)

instance Storable PhysicalDeviceFragmentShaderBarycentricPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShaderBarycentricPropertiesKHR where
  zero = PhysicalDeviceFragmentShaderBarycentricPropertiesKHR
           zero


type KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION"
pattern KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1


type KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = "VK_KHR_fragment_shader_barycentric"

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME"
pattern KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = "VK_KHR_fragment_shader_barycentric"

