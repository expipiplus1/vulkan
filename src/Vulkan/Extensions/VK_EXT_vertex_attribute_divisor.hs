{-# language CPP #-}
-- | = Name
--
-- VK_EXT_vertex_attribute_divisor - device extension
--
-- == VK_EXT_vertex_attribute_divisor
--
-- [__Name String__]
--     @VK_EXT_vertex_attribute_divisor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     191
--
-- [__Revision__]
--     3
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor VK_KHR_vertex_attribute_divisor>
--         extension
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_vertex_attribute_divisor] @vkushwaha%0A*Here describe the issue or question you have about the VK_EXT_vertex_attribute_divisor extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Faith Ekstrand, Intel
--
-- == Description
--
-- This extension allows instance-rate vertex attributes to be repeated for
-- certain number of instances instead of advancing for every instance when
-- instanced rendering is enabled.
--
-- == New Structures
--
-- -   'VertexInputBindingDivisorDescriptionEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexAttributeDivisorFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceVertexAttributeDivisorPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo':
--
--     -   'PipelineVertexInputDivisorStateCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME'
--
-- -   'EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) What is the effect of a non-zero value for @firstInstance@?
--
-- __RESOLVED__: The Vulkan API should follow the OpenGL convention and
-- offset attribute fetching by @firstInstance@ while computing vertex
-- attribute offsets.
--
-- 2) Should zero be an allowed divisor?
--
-- __RESOLVED__: Yes. A zero divisor means the vertex attribute is repeated
-- for all instances.
--
-- == Examples
--
-- To create a vertex binding such that the first binding uses instanced
-- rendering and the same attribute is used for every 4 draw instances, an
-- application could use the following set of structures:
--
-- >     const VkVertexInputBindingDivisorDescriptionEXT divisorDesc =
-- >     {
-- >         .binding = 0,
-- >         .divisor = 4
-- >     };
-- >
-- >     const VkPipelineVertexInputDivisorStateCreateInfoEXT divisorInfo =
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .vertexBindingDivisorCount = 1,
-- >         .pVertexBindingDivisors = &divisorDesc
-- >     }
-- >
-- >     const VkVertexInputBindingDescription binding =
-- >     {
-- >         .binding = 0,
-- >         .stride = sizeof(Vertex),
-- >         .inputRate = VK_VERTEX_INPUT_RATE_INSTANCE
-- >     };
-- >
-- >     const VkPipelineVertexInputStateCreateInfo viInfo =
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_CREATE_INFO,
-- >         .pNext = &divisorInfo,
-- >         ...
-- >     };
-- >     //...
--
-- == Version History
--
-- -   Revision 1, 2017-12-04 (Vikram Kushwaha)
--
--     -   First Version
--
-- -   Revision 2, 2018-07-16 (Faith Ekstrand)
--
--     -   Adjust the interaction between @divisor@ and @firstInstance@ to
--         match the OpenGL convention.
--
--     -   Disallow divisors of zero.
--
-- -   Revision 3, 2018-08-03 (Vikram Kushwaha)
--
--     -   Allow a zero divisor.
--
--     -   Add a physical device features structure to query\/enable this
--         feature.
--
-- == See Also
--
-- 'PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
-- 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT',
-- 'PipelineVertexInputDivisorStateCreateInfoEXT',
-- 'VertexInputBindingDivisorDescriptionEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_vertex_attribute_divisor  ( pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
                                                          , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
                                                          , PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
                                                          , VertexInputBindingDivisorDescriptionEXT
                                                          , PipelineVertexInputDivisorStateCreateInfoEXT
                                                          , PhysicalDeviceVertexAttributeDivisorFeaturesEXT
                                                          , EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          , pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          , VertexInputBindingDivisorDescriptionKHR(..)
                                                          , PipelineVertexInputDivisorStateCreateInfoKHR(..)
                                                          , PhysicalDeviceVertexAttributeDivisorFeaturesKHR(..)
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
import Vulkan.Extensions.VK_KHR_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesKHR)
import Vulkan.Extensions.VK_KHR_vertex_attribute_divisor (PipelineVertexInputDivisorStateCreateInfoKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_vertex_attribute_divisor (VertexInputBindingDivisorDescriptionKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR))
import Vulkan.Extensions.VK_KHR_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesKHR(..))
import Vulkan.Extensions.VK_KHR_vertex_attribute_divisor (PipelineVertexInputDivisorStateCreateInfoKHR(..))
import Vulkan.Extensions.VK_KHR_vertex_attribute_divisor (VertexInputBindingDivisorDescriptionKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR


-- | VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT - Structure
-- describing max value of vertex attribute divisor that can be supported
-- by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceVertexAttributeDivisorPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_attribute_divisor VK_EXT_vertex_attribute_divisor>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVertexAttributeDivisorPropertiesEXT = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- | #extension-limits-maxVertexAttribDivisor# @maxVertexAttribDivisor@ is
    -- the maximum value of the number of instances that will repeat the value
    -- of vertex attribute data when instanced rendering is enabled.
    maxVertexAttribDivisor :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceVertexAttributeDivisorPropertiesEXT

instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxVertexAttribDivisor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  peekCStruct p = do
    maxVertexAttribDivisor <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceVertexAttributeDivisorPropertiesEXT
             maxVertexAttribDivisor

instance Storable PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
           zero


-- No documentation found for TopLevel "VkVertexInputBindingDivisorDescriptionEXT"
type VertexInputBindingDivisorDescriptionEXT = VertexInputBindingDivisorDescriptionKHR


-- No documentation found for TopLevel "VkPipelineVertexInputDivisorStateCreateInfoEXT"
type PipelineVertexInputDivisorStateCreateInfoEXT = PipelineVertexInputDivisorStateCreateInfoKHR


-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT"
type PhysicalDeviceVertexAttributeDivisorFeaturesEXT = PhysicalDeviceVertexAttributeDivisorFeaturesKHR


type EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 3


type EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"

