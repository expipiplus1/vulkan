{-# language CPP #-}
-- | = Name
--
-- VK_NVX_multiview_per_view_attributes - device extension
--
-- == VK_NVX_multiview_per_view_attributes
--
-- [__Name String__]
--     @VK_NVX_multiview_per_view_attributes@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     98
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_multiview@
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NVX_multiview_per_view_attributes:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-01-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NVX_multiview_per_view_attributes.html SPV_NVX_multiview_per_view_attributes>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nvx/GL_NVX_multiview_per_view_attributes.txt GL_NVX_multiview_per_view_attributes>
--
--     -   This extension interacts with @VK_NV_viewport_array2@.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds a new way to write shaders to be used with multiview
-- subpasses, where the attributes for all views are written out by a
-- single invocation of the vertex processing stages. Related SPIR-V and
-- GLSL extensions @SPV_NVX_multiview_per_view_attributes@ and
-- @GL_NVX_multiview_per_view_attributes@ introduce per-view position and
-- viewport mask attributes arrays, and this extension defines how those
-- per-view attribute arrays are interpreted by Vulkan. Pipelines using
-- per-view attributes /may/ only execute the vertex processing stages once
-- for all views rather than once per-view, which reduces redundant shading
-- work.
--
-- A subpass creation flag controls whether the subpass uses this
-- extension. A subpass /must/ either exclusively use this extension or not
-- use it at all.
--
-- Some Vulkan implementations only support the position attribute varying
-- between views in the X component. A subpass can declare via a second
-- creation flag whether all pipelines compiled for this subpass will obey
-- this restriction.
--
-- Shaders that use the new per-view outputs (e.g. @gl_PositionPerViewNV@)
-- /must/ also write the non-per-view output (@gl_Position@), and the
-- values written /must/ be such that @gl_Position =
-- gl_PositionPerViewNV[gl_ViewIndex]@ for all views in the subpass.
-- Implementations are free to either use the per-view outputs or the
-- non-per-view outputs, whichever would be more efficient.
--
-- If @VK_NV_viewport_array2@ is not also supported and enabled, the
-- per-view viewport mask /must/ not be used.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX'
--
-- == New Enum Constants
--
-- -   'NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME'
--
-- -   'NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX'
--
-- == New Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-positionperview PositionPerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-viewportmaskperview ViewportMaskPerViewNV>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-PerViewAttributesNV PerViewAttributesNV>
--
-- == Examples
--
-- > #version 450 core
-- >
-- > #extension GL_KHX_multiview : enable
-- > #extension GL_NVX_multiview_per_view_attributes : enable
-- >
-- > layout(location = 0) in vec4 position;
-- > layout(set = 0, binding = 0) uniform Block { mat4 mvpPerView[2]; } buf;
-- >
-- > void main()
-- > {
-- >     // Output both per-view positions and gl_Position as a function
-- >     // of gl_ViewIndex
-- >     gl_PositionPerViewNV[0] = buf.mvpPerView[0] * position;
-- >     gl_PositionPerViewNV[1] = buf.mvpPerView[1] * position;
-- >     gl_Position = buf.mvpPerView[gl_ViewIndex] * position;
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-01-13 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NVX_multiview_per_view_attributes  ( PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
                                                               , NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
                                                               , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
                                                               , NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
                                                               , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX))
-- | VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX - Structure
-- describing multiview limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' structure
-- is included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- | #limits-perViewPositionAllComponents# @perViewPositionAllComponents@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the implementation supports
    -- per-view position values that differ in components other than the X
    -- component.
    perViewPositionAllComponents :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
#endif
deriving instance Show PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX

instance ToCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perViewPositionAllComponents))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  peekCStruct p = do
    perViewPositionAllComponents <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             (bool32ToBool perViewPositionAllComponents)

instance Storable PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
           zero


type NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: forall a . Integral a => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1


type NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

