{-# language CPP #-}
-- | = Name
--
-- VK_EXT_vertex_attribute_robustness - device extension
--
-- = VK_EXT_vertex_attribute_robustness
--
-- [__Name String__]
--     @VK_EXT_vertex_attribute_robustness@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     609
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_vertex_attribute_robustness] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_vertex_attribute_robustness extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-11-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- It can be detrimental to performance for applications to have to define
-- fake vertex attribute locations and buffer bindings for vertex shaders
-- that may reference attribute locations for which there is no vertex
-- data.
--
-- This extension allows applications to not have to specify fake vertex
-- attribute locations, and if the vertex shader reads those attributes it
-- will read (0,0,0,0) or (0,0,0,1).
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexAttributeRobustnessFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME'
--
-- -   'EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT'
--
-- == Issues
--
-- None
--
-- == Version History
--
-- -   Revision 1, 2024-11-01 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_vertex_attribute_robustness Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_vertex_attribute_robustness  ( PhysicalDeviceVertexAttributeRobustnessFeaturesEXT(..)
                                                             , EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION
                                                             , pattern EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION
                                                             , EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME
                                                             , pattern EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT))
-- | VkPhysicalDeviceVertexAttributeRobustnessFeaturesEXT - Structure
-- describing whether the vertex attribute robustness feature is supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeRobustnessFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVertexAttributeRobustnessFeaturesEXT' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_robustness VK_EXT_vertex_attribute_robustness>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeRobustnessFeaturesEXT = PhysicalDeviceVertexAttributeRobustnessFeaturesEXT
  { -- | #features-vertexAttributeRobustness# @vertexAttributeRobustness@
    -- indicates that vertex shaders /can/ read vertex attribute locations that
    -- have no vertex attribute description and the value returned is (0,0,0,0)
    -- or (0,0,0,1).
    vertexAttributeRobustness :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeRobustnessFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceVertexAttributeRobustnessFeaturesEXT

instance ToCStruct PhysicalDeviceVertexAttributeRobustnessFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeRobustnessFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (vertexAttributeRobustness))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVertexAttributeRobustnessFeaturesEXT where
  peekCStruct p = do
    vertexAttributeRobustness <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceVertexAttributeRobustnessFeaturesEXT
             (bool32ToBool vertexAttributeRobustness)

instance Storable PhysicalDeviceVertexAttributeRobustnessFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeRobustnessFeaturesEXT where
  zero = PhysicalDeviceVertexAttributeRobustnessFeaturesEXT
           zero


type EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION"
pattern EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION = 1


type EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_vertex_attribute_robustness"

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME"
pattern EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_vertex_attribute_robustness"

