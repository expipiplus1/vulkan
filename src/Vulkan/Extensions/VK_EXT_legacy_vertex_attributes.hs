{-# language CPP #-}
-- | = Name
--
-- VK_EXT_legacy_vertex_attributes - device extension
--
-- = VK_EXT_legacy_vertex_attributes
--
-- [__Name String__]
--     @VK_EXT_legacy_vertex_attributes@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     496
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_input_dynamic_state VK_EXT_vertex_input_dynamic_state>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_legacy_vertex_attributes] @zmike%0A*Here describe the issue or question you have about the VK_EXT_legacy_vertex_attributes extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_legacy_vertex_attributes.adoc VK_EXT_legacy_vertex_attributes>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
--     -   Alyssa Rosenzweig, Valve
--
-- == Description
--
-- This extension adds support for legacy features of (non-64-bit) vertex
-- attributes as found in OpenGL:
--
-- -   Vertex attributes loaded from arbitrary buffer alignments
--
-- -   Vertex attributes using arbitrary strides
--
-- -   Vertex attributes where the component data type of the binding does
--     not match the component numeric type of the shader input
--
-- These features are only usable with dynamic vertex input. Unaligned
-- loads of vertex attributes may incur performance penalties, indicated
-- with a property.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLegacyVertexAttributesFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLegacyVertexAttributesPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME'
--
-- -   'EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT'
--
-- == Issues
--
-- 1.) Should implementations convert float\/integer values?
--
-- __RESOLVED__: No. When fetching an integer data type from float values
-- or float data types from integer values, the resulting shader values are
-- implementation-dependent.
--
-- == Version History
--
-- -   Revision 1, 2024-02-16 (Mike Blumenkrantz)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_legacy_vertex_attributes Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_legacy_vertex_attributes  ( PhysicalDeviceLegacyVertexAttributesFeaturesEXT(..)
                                                          , PhysicalDeviceLegacyVertexAttributesPropertiesEXT(..)
                                                          , EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION
                                                          , pattern EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION
                                                          , EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME
                                                          , pattern EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT))
-- | VkPhysicalDeviceLegacyVertexAttributesFeaturesEXT - Structure describing
-- compatibility features for vertex attributes
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceLegacyVertexAttributesFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceLegacyVertexAttributesFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_legacy_vertex_attributes VK_EXT_legacy_vertex_attributes>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLegacyVertexAttributesFeaturesEXT = PhysicalDeviceLegacyVertexAttributesFeaturesEXT
  { -- | #features-legacyVertexAttributes# @legacyVertexAttributes@ specifies
    -- whether compatibility features for vertex attributes are supported when
    -- using dynamic vertex input state.
    legacyVertexAttributes :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLegacyVertexAttributesFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceLegacyVertexAttributesFeaturesEXT

instance ToCStruct PhysicalDeviceLegacyVertexAttributesFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLegacyVertexAttributesFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (legacyVertexAttributes))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceLegacyVertexAttributesFeaturesEXT where
  peekCStruct p = do
    legacyVertexAttributes <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceLegacyVertexAttributesFeaturesEXT
             (bool32ToBool legacyVertexAttributes)

instance Storable PhysicalDeviceLegacyVertexAttributesFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLegacyVertexAttributesFeaturesEXT where
  zero = PhysicalDeviceLegacyVertexAttributesFeaturesEXT
           zero


-- | VkPhysicalDeviceLegacyVertexAttributesPropertiesEXT - Structure
-- describing properties for legacy vertex attributes
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceLegacyVertexAttributesPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceLegacyVertexAttributesPropertiesEXT' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_legacy_vertex_attributes VK_EXT_legacy_vertex_attributes>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLegacyVertexAttributesPropertiesEXT = PhysicalDeviceLegacyVertexAttributesPropertiesEXT
  { -- | #limits-nativeUnalignedPerformance# @nativeUnalignedPerformance@
    -- specifies whether unaligned vertex fetches do not incur significant
    -- performance penalties as compared to aligned fetches.
    nativeUnalignedPerformance :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLegacyVertexAttributesPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceLegacyVertexAttributesPropertiesEXT

instance ToCStruct PhysicalDeviceLegacyVertexAttributesPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLegacyVertexAttributesPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (nativeUnalignedPerformance))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceLegacyVertexAttributesPropertiesEXT where
  peekCStruct p = do
    nativeUnalignedPerformance <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceLegacyVertexAttributesPropertiesEXT
             (bool32ToBool nativeUnalignedPerformance)

instance Storable PhysicalDeviceLegacyVertexAttributesPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLegacyVertexAttributesPropertiesEXT where
  zero = PhysicalDeviceLegacyVertexAttributesPropertiesEXT
           zero


type EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION"
pattern EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION = 1


type EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME = "VK_EXT_legacy_vertex_attributes"

-- No documentation found for TopLevel "VK_EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME"
pattern EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME = "VK_EXT_legacy_vertex_attributes"

