{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_split_barrier - device extension
--
-- = VK_EXT_shader_split_barrier
--
-- [__Name String__]
--     @VK_EXT_shader_split_barrier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     306
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_split_barrier.html SPV_EXT_split_barrier>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_split_barrier] @mnetsch%0A*Here describe the issue or question you have about the VK_EXT_shader_split_barrier extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_split_barrier.adoc VK_EXT_shader_split_barrier>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-08
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_split_barrier.txt GLSL_EXT_split_barrier>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Elina Kamenetskaya, Qualcomm Technologies, Inc.
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc.
--
--     -   John Li, Qualcomm Technologies, Inc.
--
--     -   Jeff Bolz, Nvidia
--
--     -   Ben Ashbaugh, Intel
--
-- == Description
--
-- This extension splits @OpControlBarrier@ by exposing two new barrier
-- operations with
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_split_barrier.html SPV_EXT_split_barrier>:
--
-- -   @OpControlBarrierArriveEXT@ - notifies that invocation has arrived
--     here
--
-- -   @OpControlBarrierWaitEXT@ - waits on all invocations before
--     proceeding execution
--
-- In the Vulkan context, this allows apps to synchronize Subgroup
-- execution flow within a Workgroup without requiring all Subgroups to
-- wait at the arrival condition before proceeding to execute independent
-- work. It also permits synchronizing memory access like other control
-- barriers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSplitBarrierFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderSplitBarrierPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME'
--
-- -   'EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2026-05-08 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_split_barrier Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_split_barrier  ( PhysicalDeviceShaderSplitBarrierFeaturesEXT(..)
                                                      , PhysicalDeviceShaderSplitBarrierPropertiesEXT(..)
                                                      , EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION
                                                      , pattern EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION
                                                      , EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME
                                                      , pattern EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_PROPERTIES_EXT))
-- | VkPhysicalDeviceShaderSplitBarrierFeaturesEXT - Structure describing the
-- shader split barrier features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSplitBarrierFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderSplitBarrierFeaturesEXT', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_split_barrier VK_EXT_shader_split_barrier>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSplitBarrierFeaturesEXT = PhysicalDeviceShaderSplitBarrierFeaturesEXT
  { -- | #features-shaderSplitBarrier# @shaderSplitBarrier@ indicates that the
    -- implementation supports the @SplitBarrierEXT@ SPIR-V capability.
    shaderSplitBarrier :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSplitBarrierFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderSplitBarrierFeaturesEXT

instance ToCStruct PhysicalDeviceShaderSplitBarrierFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSplitBarrierFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSplitBarrier))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSplitBarrierFeaturesEXT where
  peekCStruct p = do
    shaderSplitBarrier <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSplitBarrierFeaturesEXT
             (bool32ToBool shaderSplitBarrier)

instance Storable PhysicalDeviceShaderSplitBarrierFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSplitBarrierFeaturesEXT where
  zero = PhysicalDeviceShaderSplitBarrierFeaturesEXT
           zero


-- | VkPhysicalDeviceShaderSplitBarrierPropertiesEXT - Structure describing
-- split barrier properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSplitBarrierPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_split_barrier VK_EXT_shader_split_barrier>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSplitBarrierPropertiesEXT = PhysicalDeviceShaderSplitBarrierPropertiesEXT
  { -- | #limits-splitBarrierReservedSharedMemory#
    -- @splitBarrierReservedSharedMemory@ is the number of bytes of shared
    -- memory reserved for the implementation when the module executes
    -- @OpControlBarrierArriveEXT@ or @OpControlBarrierWaitEXT@ instructions
    splitBarrierReservedSharedMemory :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSplitBarrierPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceShaderSplitBarrierPropertiesEXT

instance ToCStruct PhysicalDeviceShaderSplitBarrierPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSplitBarrierPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (splitBarrierReservedSharedMemory)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderSplitBarrierPropertiesEXT where
  peekCStruct p = do
    splitBarrierReservedSharedMemory <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceShaderSplitBarrierPropertiesEXT
             splitBarrierReservedSharedMemory

instance Storable PhysicalDeviceShaderSplitBarrierPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSplitBarrierPropertiesEXT where
  zero = PhysicalDeviceShaderSplitBarrierPropertiesEXT
           zero


type EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION"
pattern EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION = 1


type EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME = "VK_EXT_shader_split_barrier"

-- No documentation found for TopLevel "VK_EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME"
pattern EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME = "VK_EXT_shader_split_barrier"

