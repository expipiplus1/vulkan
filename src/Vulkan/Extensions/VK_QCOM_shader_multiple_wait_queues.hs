{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_shader_multiple_wait_queues - device extension
--
-- = VK_QCOM_shader_multiple_wait_queues
--
-- [__Name String__]
--     @VK_QCOM_shader_multiple_wait_queues@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     305
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_multiple_wait_queues.html SPV_QCOM_multiple_wait_queues>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_shader_multiple_wait_queues] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_shader_multiple_wait_queues extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_shader_multiple_wait_queues.adoc VK_QCOM_shader_multiple_wait_queues>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-04
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_multiple_wait_queues.txt GLSL_QCOM_multiple_wait_queues>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Elina Kamenetskaya, Qualcomm Technologies, Inc.
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension adds a new loop control hint to the SPIR-V execution
-- environment, instructing the compiler that it should use multiple wait
-- queues to optimize a loop.
--
-- This can improve performance for loops that have high latency
-- instructions such as for @VK_KHR_cooperative_matrix@ operations, by
-- allowing the compiler to issue instructions for future iterations while
-- waiting for the current iteration to complete.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME'
--
-- -   'QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_PROPERTIES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2026-05-04 (Matthew Netsch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_shader_multiple_wait_queues Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_shader_multiple_wait_queues  ( PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM(..)
                                                              , PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM(..)
                                                              , QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION
                                                              , pattern QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION
                                                              , QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME
                                                              , pattern QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_PROPERTIES_QCOM))
-- | VkPhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM - Structure
-- describing whether shader multiple wait queues can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM', it /must/ add an
-- instance of the structure, with the desired feature members set to
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_shader_multiple_wait_queues VK_QCOM_shader_multiple_wait_queues>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM = PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM
  { -- | #features-shaderMultipleWaitQueues# @shaderMultipleWaitQueues@ indicates
    -- that the implementation supports SPIR-V modules declaring the
    -- @MultipleWaitQueuesQCOM@ capability.
    shaderMultipleWaitQueues :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM

instance ToCStruct PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderMultipleWaitQueues))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM where
  peekCStruct p = do
    shaderMultipleWaitQueues <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM
             (bool32ToBool shaderMultipleWaitQueues)

instance Storable PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM where
  zero = PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM
           zero


-- | VkPhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM - Structure
-- describing shader multiple wait queues properties that can be supported
-- by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM' structure
-- is included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_shader_multiple_wait_queues VK_QCOM_shader_multiple_wait_queues>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM = PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM
  { -- | #limits-maxShaderWaitQueues# @maxShaderWaitQueues@ is the maximum number
    -- of wait queues that /can/ be set for a @MultipleWaitQueuesQCOM@ loop
    -- control hint.
    maxShaderWaitQueues :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM

instance ToCStruct PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxShaderWaitQueues)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM where
  peekCStruct p = do
    maxShaderWaitQueues <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM
             maxShaderWaitQueues

instance Storable PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM where
  zero = PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM
           zero


type QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION"
pattern QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION = 1


type QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME = "VK_QCOM_shader_multiple_wait_queues"

-- No documentation found for TopLevel "VK_QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME"
pattern QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME = "VK_QCOM_shader_multiple_wait_queues"

