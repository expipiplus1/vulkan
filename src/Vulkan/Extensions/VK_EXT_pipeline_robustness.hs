{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_robustness - device extension
--
-- == VK_EXT_pipeline_robustness
--
-- [__Name String__]
--     @VK_EXT_pipeline_robustness@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     69
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jarred Davies
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-12
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_EXT_robustness2@
--
--     -   Interacts with @VK_EXT_image_robustness@
--
--     -   Interacts with @VK_KHR_ray_tracing_pipeline@
--
-- [__Contributors__]
--
--     -   Jarred Davies, Imagination Technologies
--
--     -   Alex Walters, Imagination Technologies
--
--     -   Piers Daniell, NVIDIA
--
--     -   Graeme Leese, Broadcom Corporation
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Faith Ekstrand, Intel
--
--     -   Lionel Landwerlin, Intel
--
--     -   Shahbaz Youssefi, Google, Inc.
--
-- == Description
--
-- This extension allows users to request robustness on a per-pipeline
-- stage basis.
--
-- As
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- and other robustness features may have an adverse effect on performance,
-- this extension is designed to allow users to request robust behavior
-- only where it may be needed.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR':
--
--     -   'PipelineRobustnessCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineRobustnessFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePipelineRobustnessPropertiesEXT'
--
-- == New Enums
--
-- -   'PipelineRobustnessBufferBehaviorEXT'
--
-- -   'PipelineRobustnessImageBehaviorEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-07-12 (Jarred Davies)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDevicePipelineRobustnessFeaturesEXT',
-- 'PhysicalDevicePipelineRobustnessPropertiesEXT',
-- 'PipelineRobustnessBufferBehaviorEXT',
-- 'PipelineRobustnessCreateInfoEXT', 'PipelineRobustnessImageBehaviorEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_robustness Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_robustness  ( PhysicalDevicePipelineRobustnessFeaturesEXT(..)
                                                     , PipelineRobustnessCreateInfoEXT(..)
                                                     , PhysicalDevicePipelineRobustnessPropertiesEXT(..)
                                                     , PipelineRobustnessBufferBehaviorEXT( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT
                                                                                          , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT
                                                                                          , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT
                                                                                          , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT
                                                                                          , ..
                                                                                          )
                                                     , PipelineRobustnessImageBehaviorEXT( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT
                                                                                         , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT
                                                                                         , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT
                                                                                         , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT
                                                                                         , ..
                                                                                         )
                                                     , EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION
                                                     , pattern EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION
                                                     , EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME
                                                     , pattern EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME
                                                     ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT))
-- | VkPhysicalDevicePipelineRobustnessFeaturesEXT - Structure describing
-- whether an implementation supports robustness requests on a per-pipeline
-- stage granularity
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- Note
--
-- Enabling
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
-- may, on some platforms, incur a minor performance cost when
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- is disabled, even for pipelines which do not make use of any robustness
-- features. If robustness is not needed,
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
-- should not be enabled by an application.
--
-- If the 'PhysicalDevicePipelineRobustnessFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePipelineRobustnessFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineRobustnessFeaturesEXT = PhysicalDevicePipelineRobustnessFeaturesEXT
  { -- | #features-pipelineRobustness# @pipelineRobustness@ indicates that
    -- robustness /can/ be requested on a per-pipeline-stage granularity.
    pipelineRobustness :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineRobustnessFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePipelineRobustnessFeaturesEXT

instance ToCStruct PhysicalDevicePipelineRobustnessFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineRobustnessFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineRobustness))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineRobustnessFeaturesEXT where
  peekCStruct p = do
    pipelineRobustness <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineRobustnessFeaturesEXT
             (bool32ToBool pipelineRobustness)

instance Storable PhysicalDevicePipelineRobustnessFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineRobustnessFeaturesEXT where
  zero = PhysicalDevicePipelineRobustnessFeaturesEXT
           zero


-- | VkPipelineRobustnessCreateInfoEXT - Structure controlling the robustness
-- of a newly created pipeline shader stage
--
-- = Description
--
-- Resources bound as
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT' will
-- have the robustness behavior that covers its active descriptor type.
--
-- The scope of the effect of 'PipelineRobustnessCreateInfoEXT' depends on
-- which structure’s @pNext@ chain it is included in.
--
-- -   'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo':
--     The robustness behavior described by
--     'PipelineRobustnessCreateInfoEXT' applies to all accesses through
--     this pipeline
--
-- -   'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo':
--     The robustness behavior described by
--     'PipelineRobustnessCreateInfoEXT' applies to all accesses emanating
--     from the shader code of this shader stage
--
-- If 'PipelineRobustnessCreateInfoEXT' is specified for both a pipeline
-- and a pipeline stage, the 'PipelineRobustnessCreateInfoEXT' specified
-- for the pipeline stage will take precedence.
--
-- When 'PipelineRobustnessCreateInfoEXT' is specified for a pipeline, it
-- only affects the subset of the pipeline that is specified by the create
-- info, as opposed to subsets linked from pipeline libraries. For
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', that subset is
-- specified by
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@.
-- For
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- that subset is specified by the specific stages in
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR'::@pStages@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-pipelineRobustness-06926# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @storageBuffers@ /must/ be
--     'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-pipelineRobustness-06927# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @uniformBuffers@ /must/ be
--     'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-pipelineRobustness-06928# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @vertexInputs@ /must/ be
--     'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-pipelineRobustness-06929# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @images@ /must/ be
--     'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-robustImageAccess-06930# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustImageAccess robustImageAccess>
--     feature is not supported, @images@ /must/ not be
--     'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-robustBufferAccess2-06931#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not supported, @storageBuffers@ /must/ not be
--     'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-robustBufferAccess2-06932#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not supported, @uniformBuffers@ /must/ not be
--     'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-robustBufferAccess2-06933#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not supported, @vertexInputs@ /must/ not be
--     'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-robustImageAccess2-06934# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustImageAccess2 robustImageAccess2>
--     feature is not supported, @images@ /must/ not be
--     'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-storageBuffers-parameter#
--     @storageBuffers@ /must/ be a valid
--     'PipelineRobustnessBufferBehaviorEXT' value
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-uniformBuffers-parameter#
--     @uniformBuffers@ /must/ be a valid
--     'PipelineRobustnessBufferBehaviorEXT' value
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-vertexInputs-parameter#
--     @vertexInputs@ /must/ be a valid
--     'PipelineRobustnessBufferBehaviorEXT' value
--
-- -   #VUID-VkPipelineRobustnessCreateInfoEXT-images-parameter# @images@
--     /must/ be a valid 'PipelineRobustnessImageBehaviorEXT' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- 'PipelineRobustnessBufferBehaviorEXT',
-- 'PipelineRobustnessImageBehaviorEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRobustnessCreateInfoEXT = PipelineRobustnessCreateInfoEXT
  { -- | @storageBuffers@ sets the behaviour of out of bounds accesses made to
    -- resources bound as:
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    storageBuffers :: PipelineRobustnessBufferBehaviorEXT
  , -- | @uniformBuffers@ describes the behaviour of out of bounds accesses made
    -- to resources bound as:
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    uniformBuffers :: PipelineRobustnessBufferBehaviorEXT
  , -- | @vertexInputs@ describes the behaviour of out of bounds accesses made to
    -- vertex input attributes
    vertexInputs :: PipelineRobustnessBufferBehaviorEXT
  , -- | @images@ describes the behaviour of out of bounds accesses made to
    -- resources bound as:
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    images :: PipelineRobustnessImageBehaviorEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRobustnessCreateInfoEXT)
#endif
deriving instance Show PipelineRobustnessCreateInfoEXT

instance ToCStruct PipelineRobustnessCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRobustnessCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (storageBuffers)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (uniformBuffers)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (vertexInputs)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehaviorEXT)) (images)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehaviorEXT)) (zero)
    f

instance FromCStruct PipelineRobustnessCreateInfoEXT where
  peekCStruct p = do
    storageBuffers <- peek @PipelineRobustnessBufferBehaviorEXT ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehaviorEXT))
    uniformBuffers <- peek @PipelineRobustnessBufferBehaviorEXT ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehaviorEXT))
    vertexInputs <- peek @PipelineRobustnessBufferBehaviorEXT ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehaviorEXT))
    images <- peek @PipelineRobustnessImageBehaviorEXT ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehaviorEXT))
    pure $ PipelineRobustnessCreateInfoEXT
             storageBuffers uniformBuffers vertexInputs images

instance Storable PipelineRobustnessCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRobustnessCreateInfoEXT where
  zero = PipelineRobustnessCreateInfoEXT
           zero
           zero
           zero
           zero


-- | VkPhysicalDevicePipelineRobustnessPropertiesEXT - Structure describing
-- the default robustness behavior of a physical device
--
-- = Description
--
-- Some implementations of Vulkan may be able to guarantee that certain
-- types of accesses are always performed with robustness even when the
-- Vulkan API’s robustness features are not explicitly enabled.
--
-- Even when an implementation reports that accesses to a given resource
-- type are robust by default, it remains invalid to make an out of bounds
-- access without requesting the appropriate robustness feature.
--
-- If the 'PhysicalDevicePipelineRobustnessPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- 'PipelineRobustnessBufferBehaviorEXT',
-- 'PipelineRobustnessImageBehaviorEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineRobustnessPropertiesEXT = PhysicalDevicePipelineRobustnessPropertiesEXT
  { -- | @defaultRobustnessStorageBuffers@ describes the behaviour of out of
    -- bounds accesses made to storage buffers when no robustness features are
    -- enabled
    defaultRobustnessStorageBuffers :: PipelineRobustnessBufferBehaviorEXT
  , -- | @defaultRobustnessUniformBuffers@ describes the behaviour of out of
    -- bounds accesses made to uniform buffers when no robustness features are
    -- enabled
    defaultRobustnessUniformBuffers :: PipelineRobustnessBufferBehaviorEXT
  , -- | @defaultRobustnessVertexInputs@ describes the behaviour of out of bounds
    -- accesses made to vertex input attributes when no robustness features are
    -- enabled
    defaultRobustnessVertexInputs :: PipelineRobustnessBufferBehaviorEXT
  , -- | @defaultRobustnessImages@ describes the behaviour of out of bounds
    -- accesses made to images when no robustness features are enabled
    defaultRobustnessImages :: PipelineRobustnessImageBehaviorEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineRobustnessPropertiesEXT)
#endif
deriving instance Show PhysicalDevicePipelineRobustnessPropertiesEXT

instance ToCStruct PhysicalDevicePipelineRobustnessPropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineRobustnessPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (defaultRobustnessStorageBuffers)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (defaultRobustnessUniformBuffers)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (defaultRobustnessVertexInputs)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehaviorEXT)) (defaultRobustnessImages)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehaviorEXT)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehaviorEXT)) (zero)
    f

instance FromCStruct PhysicalDevicePipelineRobustnessPropertiesEXT where
  peekCStruct p = do
    defaultRobustnessStorageBuffers <- peek @PipelineRobustnessBufferBehaviorEXT ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehaviorEXT))
    defaultRobustnessUniformBuffers <- peek @PipelineRobustnessBufferBehaviorEXT ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehaviorEXT))
    defaultRobustnessVertexInputs <- peek @PipelineRobustnessBufferBehaviorEXT ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehaviorEXT))
    defaultRobustnessImages <- peek @PipelineRobustnessImageBehaviorEXT ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehaviorEXT))
    pure $ PhysicalDevicePipelineRobustnessPropertiesEXT
             defaultRobustnessStorageBuffers
             defaultRobustnessUniformBuffers
             defaultRobustnessVertexInputs
             defaultRobustnessImages

instance Storable PhysicalDevicePipelineRobustnessPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineRobustnessPropertiesEXT where
  zero = PhysicalDevicePipelineRobustnessPropertiesEXT
           zero
           zero
           zero
           zero


-- | VkPipelineRobustnessBufferBehaviorEXT - Enum controlling the robustness
-- of buffer accesses in a pipeline stage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- 'PhysicalDevicePipelineRobustnessPropertiesEXT',
-- 'PipelineRobustnessCreateInfoEXT'
newtype PipelineRobustnessBufferBehaviorEXT = PipelineRobustnessBufferBehaviorEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT' specifies that
-- this pipeline stage follows the behavior of robustness features that are
-- enabled on the device that created this pipeline
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT = PipelineRobustnessBufferBehaviorEXT 0

-- | 'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT' specifies that buffer
-- accesses by this pipeline stage to the relevant resource types /must/
-- not be out of bounds
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT = PipelineRobustnessBufferBehaviorEXT 1

-- | 'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT' specifies
-- that out of bounds accesses by this pipeline stage to the relevant
-- resource types behave as if the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- feature is enabled
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT = PipelineRobustnessBufferBehaviorEXT 2

-- | 'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
-- specifies that out of bounds accesses by this pipeline stage to the
-- relevant resource types behave as if the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
-- feature is enabled
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT = PipelineRobustnessBufferBehaviorEXT 3

{-# COMPLETE
  PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT
  , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT
  , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT
  , PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT ::
    PipelineRobustnessBufferBehaviorEXT
  #-}

conNamePipelineRobustnessBufferBehaviorEXT :: String
conNamePipelineRobustnessBufferBehaviorEXT = "PipelineRobustnessBufferBehaviorEXT"

enumPrefixPipelineRobustnessBufferBehaviorEXT :: String
enumPrefixPipelineRobustnessBufferBehaviorEXT = "PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_"

showTablePipelineRobustnessBufferBehaviorEXT :: [(PipelineRobustnessBufferBehaviorEXT, String)]
showTablePipelineRobustnessBufferBehaviorEXT =
  [
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT
    , "DEVICE_DEFAULT_EXT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT
    , "DISABLED_EXT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT
    , "ROBUST_BUFFER_ACCESS_EXT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT
    , "ROBUST_BUFFER_ACCESS_2_EXT"
    )
  ]

instance Show PipelineRobustnessBufferBehaviorEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineRobustnessBufferBehaviorEXT
      showTablePipelineRobustnessBufferBehaviorEXT
      conNamePipelineRobustnessBufferBehaviorEXT
      (\(PipelineRobustnessBufferBehaviorEXT x) -> x)
      (showsPrec 11)

instance Read PipelineRobustnessBufferBehaviorEXT where
  readPrec =
    enumReadPrec
      enumPrefixPipelineRobustnessBufferBehaviorEXT
      showTablePipelineRobustnessBufferBehaviorEXT
      conNamePipelineRobustnessBufferBehaviorEXT
      PipelineRobustnessBufferBehaviorEXT

-- | VkPipelineRobustnessImageBehaviorEXT - Enum controlling the robustness
-- of image accesses in a pipeline stage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- 'PhysicalDevicePipelineRobustnessPropertiesEXT',
-- 'PipelineRobustnessCreateInfoEXT'
newtype PipelineRobustnessImageBehaviorEXT = PipelineRobustnessImageBehaviorEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT' specifies that
-- this pipeline stage follows the behavior of robustness features that are
-- enabled on the device that created this pipeline
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT = PipelineRobustnessImageBehaviorEXT 0

-- | 'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT' specifies that image
-- accesses by this pipeline stage to the relevant resource types /must/
-- not be out of bounds
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT = PipelineRobustnessImageBehaviorEXT 1

-- | 'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT' specifies
-- that out of bounds accesses by this pipeline stage to images behave as
-- if the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustImageAccess robustImageAccess>
-- feature is enabled
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT = PipelineRobustnessImageBehaviorEXT 2

-- | 'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT' specifies
-- that out of bounds accesses by this pipeline stage to images behave as
-- if the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustImageAccess2 robustImageAccess2>
-- feature is enabled
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT = PipelineRobustnessImageBehaviorEXT 3

{-# COMPLETE
  PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT
  , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT
  , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT
  , PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT ::
    PipelineRobustnessImageBehaviorEXT
  #-}

conNamePipelineRobustnessImageBehaviorEXT :: String
conNamePipelineRobustnessImageBehaviorEXT = "PipelineRobustnessImageBehaviorEXT"

enumPrefixPipelineRobustnessImageBehaviorEXT :: String
enumPrefixPipelineRobustnessImageBehaviorEXT = "PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_"

showTablePipelineRobustnessImageBehaviorEXT :: [(PipelineRobustnessImageBehaviorEXT, String)]
showTablePipelineRobustnessImageBehaviorEXT =
  [
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT
    , "DEVICE_DEFAULT_EXT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT
    , "DISABLED_EXT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT
    , "ROBUST_IMAGE_ACCESS_EXT"
    )
  ,
    ( PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT
    , "ROBUST_IMAGE_ACCESS_2_EXT"
    )
  ]

instance Show PipelineRobustnessImageBehaviorEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineRobustnessImageBehaviorEXT
      showTablePipelineRobustnessImageBehaviorEXT
      conNamePipelineRobustnessImageBehaviorEXT
      (\(PipelineRobustnessImageBehaviorEXT x) -> x)
      (showsPrec 11)

instance Read PipelineRobustnessImageBehaviorEXT where
  readPrec =
    enumReadPrec
      enumPrefixPipelineRobustnessImageBehaviorEXT
      showTablePipelineRobustnessImageBehaviorEXT
      conNamePipelineRobustnessImageBehaviorEXT
      PipelineRobustnessImageBehaviorEXT

type EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION"
pattern EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION = 1


type EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_pipeline_robustness"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME"
pattern EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_pipeline_robustness"

