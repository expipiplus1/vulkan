{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'"
module Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'  ( PhysicalDevicePipelineRobustnessFeatures(..)
                                                                                       , PipelineRobustnessCreateInfo(..)
                                                                                       , PhysicalDevicePipelineRobustnessProperties(..)
                                                                                       , StructureType(..)
                                                                                       , PipelineRobustnessBufferBehavior(..)
                                                                                       , PipelineRobustnessImageBehavior(..)
                                                                                       ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior)
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO))
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior(..))
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDevicePipelineRobustnessFeatures - Structure describing
-- whether an implementation supports robustness requests on a per-pipeline
-- stage granularity
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- Enabling the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
-- feature may, on some platforms, incur a minor performance cost when the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- feature is not enabled, even for pipelines which do not make use of any
-- robustness features. If robustness is not needed, the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
-- feature should not be enabled by an application.
--
-- If the 'PhysicalDevicePipelineRobustnessFeatures' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePipelineRobustnessFeatures', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineRobustnessFeatures = PhysicalDevicePipelineRobustnessFeatures
  { -- | #extension-features-pipelineRobustness# @pipelineRobustness@ indicates
    -- that robustness /can/ be requested on a per-pipeline-stage granularity.
    pipelineRobustness :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineRobustnessFeatures)
#endif
deriving instance Show PhysicalDevicePipelineRobustnessFeatures

instance ToCStruct PhysicalDevicePipelineRobustnessFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineRobustnessFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineRobustness))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineRobustnessFeatures where
  peekCStruct p = do
    pipelineRobustness <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineRobustnessFeatures
             (bool32ToBool pipelineRobustness)

instance Storable PhysicalDevicePipelineRobustnessFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineRobustnessFeatures where
  zero = PhysicalDevicePipelineRobustnessFeatures
           zero


-- | VkPipelineRobustnessCreateInfo - Structure controlling the robustness of
-- a newly created pipeline shader stage
--
-- = Description
--
-- Resources bound as
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT' will
-- have the robustness behavior that covers its active descriptor type.
--
-- The scope of the effect of 'PipelineRobustnessCreateInfo' depends on
-- which structure’s @pNext@ chain it is included in.
--
-- -   'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
--     'Vulkan.Core10.ComputePipeline.ComputePipelineCreateInfo':
--     The robustness behavior described by 'PipelineRobustnessCreateInfo'
--     applies to all accesses through this pipeline
--
-- -   'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo':
--     The robustness behavior described by 'PipelineRobustnessCreateInfo'
--     applies to all accesses emanating from the shader code of this
--     shader stage
--
-- If 'PipelineRobustnessCreateInfo' is specified for both a pipeline and a
-- pipeline stage, the 'PipelineRobustnessCreateInfo' specified for the
-- pipeline stage will take precedence.
--
-- When 'PipelineRobustnessCreateInfo' is specified for a pipeline, it only
-- affects the subset of the pipeline that is specified by the create info,
-- as opposed to subsets linked from pipeline libraries. For
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo', that subset
-- is specified by
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@.
-- For
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- that subset is specified by the specific stages in
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR'::@pStages@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-pipelineRobustness-06926# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @storageBuffers@ /must/ be
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-pipelineRobustness-06927# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @uniformBuffers@ /must/ be
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-pipelineRobustness-06928# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @vertexInputs@ /must/ be
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-pipelineRobustness-06929# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineRobustness pipelineRobustness>
--     feature is not enabled, @images@ /must/ be
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-robustImageAccess-06930# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustImageAccess robustImageAccess>
--     feature is not supported, @images@ /must/ not be
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-robustBufferAccess2-06931# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not supported, @storageBuffers@ /must/ not be
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-robustBufferAccess2-06932# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not supported, @uniformBuffers@ /must/ not be
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-robustBufferAccess2-06933# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not supported, @vertexInputs@ /must/ not be
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-robustImageAccess2-06934# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustImageAccess2 robustImageAccess2>
--     feature is not supported, @images@ /must/ not be
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-storageBuffers-10636# If
--     @storageBuffers@ is
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2',
--     and either the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingStorageBufferUpdateAfterBind descriptorBindingStorageBufferUpdateAfterBind>
--     feature or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingStorageTexelBufferUpdateAfterBind descriptorBindingStorageTexelBufferUpdateAfterBind>
--     feature is enabled on the device,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-robustBufferAccessUpdateAfterBind robustBufferAccessUpdateAfterBind>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-uniformBuffers-10637# If
--     @uniformBuffers@ is
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2',
--     and either the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingInlineUniformBlockUpdateAfterBind descriptorBindingInlineUniformBlockUpdateAfterBind>
--     feature, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingUniformBufferUpdateAfterBind descriptorBindingUniformBufferUpdateAfterBind>
--     feature, or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingUniformTexelBufferUpdateAfterBind descriptorBindingUniformTexelBufferUpdateAfterBind>
--     feature is enabled on the device,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-robustBufferAccessUpdateAfterBind robustBufferAccessUpdateAfterBind>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-images-10638# If @images@ is
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2',
--     and either the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingStorageImageUpdateAfterBind descriptorBindingStorageImageUpdateAfterBind>
--     feature or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorBindingSampledImageUpdateAfterBind descriptorBindingSampledImageUpdateAfterBind>
--     feature is enabled on the device,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-robustBufferAccessUpdateAfterBind robustBufferAccessUpdateAfterBind>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO'
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-storageBuffers-parameter#
--     @storageBuffers@ /must/ be a valid
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior'
--     value
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-uniformBuffers-parameter#
--     @uniformBuffers@ /must/ be a valid
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior'
--     value
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-vertexInputs-parameter#
--     @vertexInputs@ /must/ be a valid
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior'
--     value
--
-- -   #VUID-VkPipelineRobustnessCreateInfo-images-parameter# @images@
--     /must/ be a valid
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PipelineRobustnessImageBehavior'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_robustness VK_EXT_pipeline_robustness>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior',
-- 'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PipelineRobustnessImageBehavior',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRobustnessCreateInfo = PipelineRobustnessCreateInfo
  { -- | @storageBuffers@ sets the behavior of out of bounds accesses made to
    -- resources bound as:
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    storageBuffers :: PipelineRobustnessBufferBehavior
  , -- | @uniformBuffers@ describes the behavior of out of bounds accesses made
    -- to resources bound as:
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    uniformBuffers :: PipelineRobustnessBufferBehavior
  , -- | @vertexInputs@ describes the behavior of out of bounds accesses made to
    -- vertex input attributes
    vertexInputs :: PipelineRobustnessBufferBehavior
  , -- | @images@ describes the behavior of out of bounds accesses made to
    -- resources bound as:
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
    --
    -- -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    images :: PipelineRobustnessImageBehavior
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRobustnessCreateInfo)
#endif
deriving instance Show PipelineRobustnessCreateInfo

instance ToCStruct PipelineRobustnessCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRobustnessCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehavior)) (storageBuffers)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehavior)) (uniformBuffers)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehavior)) (vertexInputs)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehavior)) (images)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehavior)) (zero)
    f

instance FromCStruct PipelineRobustnessCreateInfo where
  peekCStruct p = do
    storageBuffers <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehavior))
    uniformBuffers <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehavior))
    vertexInputs <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehavior))
    images <- peek @PipelineRobustnessImageBehavior ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehavior))
    pure $ PipelineRobustnessCreateInfo
             storageBuffers uniformBuffers vertexInputs images

instance Storable PipelineRobustnessCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRobustnessCreateInfo where
  zero = PipelineRobustnessCreateInfo
           zero
           zero
           zero
           zero


-- | VkPhysicalDevicePipelineRobustnessProperties - Structure describing the
-- default robustness behavior of a physical device
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
-- If the 'PhysicalDevicePipelineRobustnessProperties' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior',
-- 'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PipelineRobustnessImageBehavior',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineRobustnessProperties = PhysicalDevicePipelineRobustnessProperties
  { -- | @defaultRobustnessStorageBuffers@ describes the behavior of out of
    -- bounds accesses made to storage buffers when no robustness features are
    -- enabled
    defaultRobustnessStorageBuffers :: PipelineRobustnessBufferBehavior
  , -- | @defaultRobustnessUniformBuffers@ describes the behavior of out of
    -- bounds accesses made to uniform buffers when no robustness features are
    -- enabled
    defaultRobustnessUniformBuffers :: PipelineRobustnessBufferBehavior
  , -- | @defaultRobustnessVertexInputs@ describes the behavior of out of bounds
    -- accesses made to vertex input attributes when no robustness features are
    -- enabled
    defaultRobustnessVertexInputs :: PipelineRobustnessBufferBehavior
  , -- | @defaultRobustnessImages@ describes the behavior of out of bounds
    -- accesses made to images when no robustness features are enabled
    defaultRobustnessImages :: PipelineRobustnessImageBehavior
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineRobustnessProperties)
#endif
deriving instance Show PhysicalDevicePipelineRobustnessProperties

instance ToCStruct PhysicalDevicePipelineRobustnessProperties where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineRobustnessProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehavior)) (defaultRobustnessStorageBuffers)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehavior)) (defaultRobustnessUniformBuffers)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehavior)) (defaultRobustnessVertexInputs)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehavior)) (defaultRobustnessImages)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehavior)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehavior)) (zero)
    f

instance FromCStruct PhysicalDevicePipelineRobustnessProperties where
  peekCStruct p = do
    defaultRobustnessStorageBuffers <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 16 :: Ptr PipelineRobustnessBufferBehavior))
    defaultRobustnessUniformBuffers <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 20 :: Ptr PipelineRobustnessBufferBehavior))
    defaultRobustnessVertexInputs <- peek @PipelineRobustnessBufferBehavior ((p `plusPtr` 24 :: Ptr PipelineRobustnessBufferBehavior))
    defaultRobustnessImages <- peek @PipelineRobustnessImageBehavior ((p `plusPtr` 28 :: Ptr PipelineRobustnessImageBehavior))
    pure $ PhysicalDevicePipelineRobustnessProperties
             defaultRobustnessStorageBuffers
             defaultRobustnessUniformBuffers
             defaultRobustnessVertexInputs
             defaultRobustnessImages

instance Storable PhysicalDevicePipelineRobustnessProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineRobustnessProperties where
  zero = PhysicalDevicePipelineRobustnessProperties
           zero
           zero
           zero
           zero

