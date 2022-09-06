{-# language CPP #-}
-- No documentation found for Chapter "Core13"
module Vulkan.Core13  ( pattern API_VERSION_1_3
                      , PhysicalDeviceVulkan13Features(..)
                      , PhysicalDeviceVulkan13Properties(..)
                      , StructureType(..)
                      , Flags64
                      , module Vulkan.Core13.Enums
                      , module Vulkan.Core13.Handles
                      , module Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state
                      , module Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2
                      , module Vulkan.Core13.Promoted_From_VK_EXT_image_robustness
                      , module Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block
                      , module Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control
                      , module Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback
                      , module Vulkan.Core13.Promoted_From_VK_EXT_private_data
                      , module Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation
                      , module Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control
                      , module Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment
                      , module Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr
                      , module Vulkan.Core13.Promoted_From_VK_EXT_tooling_info
                      , module Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2
                      , module Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering
                      , module Vulkan.Core13.Promoted_From_VK_KHR_format_feature_flags2
                      , module Vulkan.Core13.Promoted_From_VK_KHR_maintenance4
                      , module Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product
                      , module Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation
                      , module Vulkan.Core13.Promoted_From_VK_KHR_synchronization2
                      , module Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory
                      ) where
import Vulkan.Core13.Enums
import Vulkan.Core13.Handles
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state
import Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2
import Vulkan.Core13.Promoted_From_VK_EXT_image_robustness
import Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block
import Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control
import Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback
import Vulkan.Core13.Promoted_From_VK_EXT_private_data
import Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation
import Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control
import Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment
import Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr
import Vulkan.Core13.Promoted_From_VK_EXT_tooling_info
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering
import Vulkan.Core13.Promoted_From_VK_KHR_format_feature_flags2
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4
import Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product
import Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2
import Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Version (pattern MAKE_API_VERSION)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_PROPERTIES))
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
pattern API_VERSION_1_3 :: Word32
pattern API_VERSION_1_3 = MAKE_API_VERSION 1 3 0


-- | VkPhysicalDeviceVulkan13Features - Structure describing the Vulkan 1.3
-- features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan13Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVulkan13Features' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan13Features = PhysicalDeviceVulkan13Features
  { -- | #features-robustImageAccess# @robustImageAccess@ indicates whether image
    -- accesses are tightly bounds-checked against the dimensions of the image
    -- view.
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-input-validation Invalid texels>
    -- resulting from out of bounds image loads will be replaced as described
    -- in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-replacement Texel Replacement>,
    -- with either (0,0,1) or (0,0,0) values inserted for missing G, B, or A
    -- components based on the format.
    robustImageAccess :: Bool
  , -- | #features-inlineUniformBlock# @inlineUniformBlock@ indicates whether the
    -- implementation supports inline uniform block descriptors. If this
    -- feature is not enabled,
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    -- /must/ not be used.
    inlineUniformBlock :: Bool
  , -- | #features-descriptorBindingInlineUniformBlockUpdateAfterBind#
    -- @descriptorBindingInlineUniformBlockUpdateAfterBind@ indicates whether
    -- the implementation supports updating inline uniform block descriptors
    -- after a set is bound. If this feature is not enabled,
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'.
    descriptorBindingInlineUniformBlockUpdateAfterBind :: Bool
  , -- | #features-pipelineCreationCacheControl# @pipelineCreationCacheControl@
    -- indicates that the implementation supports:
    --
    -- -   The following /can/ be used in @Vk*PipelineCreateInfo@::@flags@:
    --
    --     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
    --
    --     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
    --
    -- -   The following /can/ be used in
    --     'Vulkan.Core10.PipelineCache.PipelineCacheCreateInfo'::@flags@:
    --
    --     -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT'
    pipelineCreationCacheControl :: Bool
  , -- | #features-privateData# @privateData@ indicates whether the
    -- implementation supports private data. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#private-data Private Data>.
    privateData :: Bool
  , -- | #features-shaderDemoteToHelperInvocation#
    -- @shaderDemoteToHelperInvocation@ indicates whether the implementation
    -- supports the SPIR-V @DemoteToHelperInvocationEXT@ capability.
    shaderDemoteToHelperInvocation :: Bool
  , -- | #features-shaderTerminateInvocation# @shaderTerminateInvocation@
    -- specifies whether the implementation supports SPIR-V modules that use
    -- the @SPV_KHR_terminate_invocation@ extension.
    shaderTerminateInvocation :: Bool
  , -- | #features-subgroupSizeControl# @subgroupSizeControl@ indicates whether
    -- the implementation supports controlling shader subgroup sizes via the
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT'
    -- flag and the
    -- 'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
    -- structure.
    subgroupSizeControl :: Bool
  , -- | #features-computeFullSubgroups# @computeFullSubgroups@ indicates whether
    -- the implementation supports requiring full subgroups in compute , mesh,
    -- or task shaders via the
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT'
    -- flag.
    computeFullSubgroups :: Bool
  , -- | #features-synchronization2# @synchronization2@ indicates whether the
    -- implementation supports the new set of synchronization commands
    -- introduced in @VK_KHR_synchronization2@.
    synchronization2 :: Bool
  , -- | #features-textureCompressionASTC_HDR# @textureCompressionASTC_HDR@
    -- indicates whether all of the ASTC HDR compressed texture formats are
    -- supported. If this feature is enabled, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x6_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x8_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x6_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x8_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x10_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x10_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x12_SFLOAT_BLOCK'
    --
    -- To query for additional properties, or if the feature is not enabled,
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
    -- and
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
    -- /can/ be used to check for supported properties of individual formats as
    -- normal.
    textureCompressionASTC_HDR :: Bool
  , -- | #features-shaderZeroInitializeWorkgroupMemory#
    -- @shaderZeroInitializeWorkgroupMemory@ specifies whether the
    -- implementation supports initializing a variable in Workgroup storage
    -- class.
    shaderZeroInitializeWorkgroupMemory :: Bool
  , -- | #features-dynamicRendering# @dynamicRendering@ specifies that the
    -- implementation supports dynamic render pass instances using the
    -- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
    -- command.
    dynamicRendering :: Bool
  , -- | #features-shaderIntegerDotProduct# @shaderIntegerDotProduct@ specifies
    -- whether shader modules /can/ declare the @DotProductInputAllKHR@,
    -- @DotProductInput4x8BitKHR@, @DotProductInput4x8BitPackedKHR@ and
    -- @DotProductKHR@ capabilities.
    shaderIntegerDotProduct :: Bool
  , -- | #features-maintenance4# @maintenance4@ indicates that the implementation
    -- supports the following:
    --
    -- -   The application /may/ destroy a
    --     'Vulkan.Core10.Handles.PipelineLayout' object immediately after
    --     using it to create another object.
    --
    -- -   @LocalSizeId@ /can/ be used as an alternative to @LocalSize@ to
    --     specify the local workgroup size with specialization constants.
    --
    -- -   Images created with identical creation parameters will always have
    --     the same alignment requirements.
    --
    -- -   The size memory requirement of a buffer or image is never greater
    --     than that of another buffer or image created with a greater or equal
    --     size.
    --
    -- -   Push constants do not have to be initialized before they are
    --     dynamically accessed.
    --
    -- -   The interface matching rules allow a larger output vector to match
    --     with a smaller input vector, with additional values being discarded.
    maintenance4 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan13Features)
#endif
deriving instance Show PhysicalDeviceVulkan13Features

instance ToCStruct PhysicalDeviceVulkan13Features where
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan13Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (robustImageAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (inlineUniformBlock))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (descriptorBindingInlineUniformBlockUpdateAfterBind))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (pipelineCreationCacheControl))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (privateData))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderDemoteToHelperInvocation))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderTerminateInvocation))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (subgroupSizeControl))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (computeFullSubgroups))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (synchronization2))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (textureCompressionASTC_HDR))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (shaderZeroInitializeWorkgroupMemory))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (dynamicRendering))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (shaderIntegerDotProduct))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (maintenance4))
    f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVulkan13Features where
  peekCStruct p = do
    robustImageAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    inlineUniformBlock <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    descriptorBindingInlineUniformBlockUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pipelineCreationCacheControl <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    privateData <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderDemoteToHelperInvocation <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderTerminateInvocation <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    subgroupSizeControl <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    computeFullSubgroups <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    synchronization2 <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    textureCompressionASTC_HDR <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    shaderZeroInitializeWorkgroupMemory <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    dynamicRendering <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    shaderIntegerDotProduct <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    maintenance4 <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkan13Features
             (bool32ToBool robustImageAccess) (bool32ToBool inlineUniformBlock) (bool32ToBool descriptorBindingInlineUniformBlockUpdateAfterBind) (bool32ToBool pipelineCreationCacheControl) (bool32ToBool privateData) (bool32ToBool shaderDemoteToHelperInvocation) (bool32ToBool shaderTerminateInvocation) (bool32ToBool subgroupSizeControl) (bool32ToBool computeFullSubgroups) (bool32ToBool synchronization2) (bool32ToBool textureCompressionASTC_HDR) (bool32ToBool shaderZeroInitializeWorkgroupMemory) (bool32ToBool dynamicRendering) (bool32ToBool shaderIntegerDotProduct) (bool32ToBool maintenance4)

instance Storable PhysicalDeviceVulkan13Features where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan13Features where
  zero = PhysicalDeviceVulkan13Features
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceVulkan13Properties - Structure specifying physical
-- device properties for functionality promoted to Vulkan 1.3
--
-- = Description
--
-- If the 'PhysicalDeviceVulkan13Properties' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- These properties correspond to Vulkan 1.3 functionality.
--
-- The members of 'PhysicalDeviceVulkan13Properties' /must/ have the same
-- values as the corresponding members of
-- 'Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockProperties'
-- and
-- 'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlProperties'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkan13Properties = PhysicalDeviceVulkan13Properties
  { -- | #limits-minSubgroupSize# @minSubgroupSize@ is the minimum subgroup size
    -- supported by this device. @minSubgroupSize@ is at least one if any of
    -- the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @minSubgroupSize@
    -- is a power-of-two. @minSubgroupSize@ is less than or equal to
    -- @maxSubgroupSize@. @minSubgroupSize@ is less than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    minSubgroupSize :: Word32
  , -- | #limits-maxSubgroupSize# @maxSubgroupSize@ is the maximum subgroup size
    -- supported by this device. @maxSubgroupSize@ is at least one if any of
    -- the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @maxSubgroupSize@
    -- is a power-of-two. @maxSubgroupSize@ is greater than or equal to
    -- @minSubgroupSize@. @maxSubgroupSize@ is greater than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    maxSubgroupSize :: Word32
  , -- | #limits-maxComputeWorkgroupSubgroups# @maxComputeWorkgroupSubgroups@ is
    -- the maximum number of subgroups supported by the implementation within a
    -- workgroup.
    maxComputeWorkgroupSubgroups :: Word32
  , -- | #limits-requiredSubgroupSizeStages# @requiredSubgroupSizeStages@ is a
    -- bitfield of what shader stages support having a required subgroup size
    -- specified.
    requiredSubgroupSizeStages :: ShaderStageFlags
  , -- | #limits-maxInlineUniformBlockSize# @maxInlineUniformBlockSize@ is the
    -- maximum size in bytes of an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>
    -- binding.
    maxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkan13Properties" "maxPerStageDescriptorInlineUniformBlocks"
    maxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks#
    -- @maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxPerStageDescriptorInlineUniformBlocks@ but counts descriptor
    -- bindings from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- | #limits-maxDescriptorSetInlineUniformBlocks#
    -- @maxDescriptorSetInlineUniformBlocks@ is the maximum number of inline
    -- uniform block bindings that /can/ be included in descriptor bindings in
    -- a pipeline layout across all pipeline shader stages and descriptor set
    -- numbers. Descriptor bindings with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    -- count against this limit. Only descriptor bindings in descriptor set
    -- layouts created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxDescriptorSetInlineUniformBlocks :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindInlineUniformBlocks#
    -- @maxDescriptorSetUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxDescriptorSetInlineUniformBlocks@ but counts descriptor bindings
    -- from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  , -- | #limits-maxInlineUniformTotalSize# @maxInlineUniformTotalSize@ is the
    -- maximum total size in bytes of all inline uniform block bindings, across
    -- all pipeline shader stages and descriptor set numbers, that /can/ be
    -- included in a pipeline layout. Descriptor bindings with a descriptor
    -- type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    -- count against this limit.
    maxInlineUniformTotalSize :: Word32
  , -- | @integerDotProduct8BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct8BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct8BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct8BitSignedAccelerated :: Bool
  , -- | @integerDotProduct8BitMixedSignednessAccelerated@ is a boolean that will
    -- be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit mixed
    -- signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct8BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct4x8BitPackedUnsignedAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit
    -- unsigned dot product operations from operands packed into 32-bit
    -- integers using the @OpUDotKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct4x8BitPackedUnsignedAccelerated :: Bool
  , -- | @integerDotProduct4x8BitPackedSignedAccelerated@ is a boolean that will
    -- be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 8-bit signed
    -- dot product operations from operands packed into 32-bit integers using
    -- the @OpSDotKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct4x8BitPackedSignedAccelerated :: Bool
  , -- | @integerDotProduct4x8BitPackedMixedSignednessAccelerated@ is a boolean
    -- that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for
    -- 8-bit mixed signedness dot product operations from operands packed into
    -- 32-bit integers using the @OpSUDotKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct4x8BitPackedMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct16BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 16-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct16BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct16BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 16-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct16BitSignedAccelerated :: Bool
  , -- | @integerDotProduct16BitMixedSignednessAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 16-bit
    -- mixed signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct16BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct32BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 32-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct32BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct32BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 32-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct32BitSignedAccelerated :: Bool
  , -- | @integerDotProduct32BitMixedSignednessAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 32-bit
    -- mixed signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct32BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProduct64BitUnsignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 64-bit unsigned
    -- dot product operations using the @OpUDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct64BitUnsignedAccelerated :: Bool
  , -- | @integerDotProduct64BitSignedAccelerated@ is a boolean that will be
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 64-bit signed
    -- dot product operations using the @OpSDotKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct64BitSignedAccelerated :: Bool
  , -- | @integerDotProduct64BitMixedSignednessAccelerated@ is a boolean that
    -- will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the support for 64-bit
    -- mixed signedness dot product operations using the @OpSUDotKHR@ SPIR-V
    -- instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProduct64BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating8BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating8BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating8BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating8BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit unsigned accumulating saturating dot product
    -- operations from operands packed into 32-bit integers using the
    -- @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit signed accumulating saturating dot product operations
    -- from operands packed into 32-bit integers using the @OpSDotAccSatKHR@
    -- SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 8-bit mixed signedness accumulating saturating dot product
    -- operations from operands packed into 32-bit integers using the
    -- @OpSUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating16BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 16-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating16BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating16BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 16-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating16BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 16-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating32BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 32-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating32BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating32BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 32-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating32BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 32-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating64BitUnsignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 64-bit unsigned accumulating saturating dot product
    -- operations using the @OpUDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating64BitUnsignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating64BitSignedAccelerated@ is a
    -- boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 64-bit signed accumulating saturating dot product operations
    -- using the @OpSDotAccSatKHR@ SPIR-V instruction is accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating64BitSignedAccelerated :: Bool
  , -- | @integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated@
    -- is a boolean that will be 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- support for 64-bit mixed signedness accumulating saturating dot product
    -- operations using the @OpSUDotAccSatKHR@ SPIR-V instruction is
    -- accelerated
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-integer-dot-product-accelerated as defined below>.
    integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated :: Bool
  , -- | #limits-storageTexelBufferOffsetAlignmentBytes#
    -- @storageTexelBufferOffsetAlignmentBytes@ is a byte alignment that is
    -- sufficient for a storage texel buffer of any format. The value /must/ be
    -- a power of two.
    storageTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- | #limits-storageTexelBufferOffsetSingleTexelAlignment#
    -- @storageTexelBufferOffsetSingleTexelAlignment@ indicates whether single
    -- texel alignment is sufficient for a storage texel buffer of any format.
    storageTexelBufferOffsetSingleTexelAlignment :: Bool
  , -- | #limits-uniformTexelBufferOffsetAlignmentBytes#
    -- @uniformTexelBufferOffsetAlignmentBytes@ is a byte alignment that is
    -- sufficient for a uniform texel buffer of any format. The value /must/ be
    -- a power of two.
    uniformTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- | #limits-uniformTexelBufferOffsetSingleTexelAlignment#
    -- @uniformTexelBufferOffsetSingleTexelAlignment@ indicates whether single
    -- texel alignment is sufficient for a uniform texel buffer of any format.
    uniformTexelBufferOffsetSingleTexelAlignment :: Bool
  , -- | #limits-maxBufferSize# @maxBufferSize@ is the maximum size
    -- 'Vulkan.Core10.Handles.Buffer' that /can/ be created.
    maxBufferSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkan13Properties)
#endif
deriving instance Show PhysicalDeviceVulkan13Properties

instance ToCStruct PhysicalDeviceVulkan13Properties where
  withCStruct x f = allocaBytes 216 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkan13Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minSubgroupSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxSubgroupSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxComputeWorkgroupSubgroups)
    poke ((p `plusPtr` 28 :: Ptr ShaderStageFlags)) (requiredSubgroupSizeStages)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxInlineUniformBlockSize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxPerStageDescriptorInlineUniformBlocks)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxDescriptorSetInlineUniformBlocks)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindInlineUniformBlocks)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxInlineUniformTotalSize)
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (integerDotProduct8BitUnsignedAccelerated))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (integerDotProduct8BitSignedAccelerated))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (integerDotProduct8BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (integerDotProduct4x8BitPackedUnsignedAccelerated))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (integerDotProduct4x8BitPackedSignedAccelerated))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (integerDotProduct4x8BitPackedMixedSignednessAccelerated))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (integerDotProduct16BitUnsignedAccelerated))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (integerDotProduct16BitSignedAccelerated))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (integerDotProduct16BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (integerDotProduct32BitUnsignedAccelerated))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (integerDotProduct32BitSignedAccelerated))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (integerDotProduct32BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (integerDotProduct64BitUnsignedAccelerated))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (integerDotProduct64BitSignedAccelerated))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (integerDotProduct64BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating8BitUnsignedAccelerated))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating8BitSignedAccelerated))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated))
    poke ((p `plusPtr` 140 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating16BitUnsignedAccelerated))
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating16BitSignedAccelerated))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 152 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating32BitUnsignedAccelerated))
    poke ((p `plusPtr` 156 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating32BitSignedAccelerated))
    poke ((p `plusPtr` 160 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 164 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating64BitUnsignedAccelerated))
    poke ((p `plusPtr` 168 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating64BitSignedAccelerated))
    poke ((p `plusPtr` 172 :: Ptr Bool32)) (boolToBool32 (integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated))
    poke ((p `plusPtr` 176 :: Ptr DeviceSize)) (storageTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 184 :: Ptr Bool32)) (boolToBool32 (storageTexelBufferOffsetSingleTexelAlignment))
    poke ((p `plusPtr` 192 :: Ptr DeviceSize)) (uniformTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 200 :: Ptr Bool32)) (boolToBool32 (uniformTexelBufferOffsetSingleTexelAlignment))
    poke ((p `plusPtr` 208 :: Ptr DeviceSize)) (maxBufferSize)
    f
  cStructSize = 216
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 140 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 152 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 156 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 160 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 164 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 168 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 172 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 176 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 184 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 192 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 200 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 208 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceVulkan13Properties where
  peekCStruct p = do
    minSubgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxSubgroupSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxComputeWorkgroupSubgroups <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    requiredSubgroupSizeStages <- peek @ShaderStageFlags ((p `plusPtr` 28 :: Ptr ShaderStageFlags))
    maxInlineUniformBlockSize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxPerStageDescriptorInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxDescriptorSetInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxInlineUniformTotalSize <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    integerDotProduct8BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    integerDotProduct8BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    integerDotProduct8BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    integerDotProduct4x8BitPackedUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    integerDotProduct4x8BitPackedSignedAccelerated <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    integerDotProduct4x8BitPackedMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    integerDotProduct16BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    integerDotProduct16BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    integerDotProduct16BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    integerDotProduct32BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    integerDotProduct32BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 96 :: Ptr Bool32))
    integerDotProduct32BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 100 :: Ptr Bool32))
    integerDotProduct64BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 104 :: Ptr Bool32))
    integerDotProduct64BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 108 :: Ptr Bool32))
    integerDotProduct64BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 112 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating8BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 116 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating8BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 120 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 124 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 128 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated <- peek @Bool32 ((p `plusPtr` 132 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 136 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating16BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 140 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating16BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 144 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 148 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating32BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 152 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating32BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 156 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 160 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating64BitUnsignedAccelerated <- peek @Bool32 ((p `plusPtr` 164 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating64BitSignedAccelerated <- peek @Bool32 ((p `plusPtr` 168 :: Ptr Bool32))
    integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated <- peek @Bool32 ((p `plusPtr` 172 :: Ptr Bool32))
    storageTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 176 :: Ptr DeviceSize))
    storageTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 184 :: Ptr Bool32))
    uniformTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 192 :: Ptr DeviceSize))
    uniformTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 200 :: Ptr Bool32))
    maxBufferSize <- peek @DeviceSize ((p `plusPtr` 208 :: Ptr DeviceSize))
    pure $ PhysicalDeviceVulkan13Properties
             minSubgroupSize maxSubgroupSize maxComputeWorkgroupSubgroups requiredSubgroupSizeStages maxInlineUniformBlockSize maxPerStageDescriptorInlineUniformBlocks maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks maxDescriptorSetInlineUniformBlocks maxDescriptorSetUpdateAfterBindInlineUniformBlocks maxInlineUniformTotalSize (bool32ToBool integerDotProduct8BitUnsignedAccelerated) (bool32ToBool integerDotProduct8BitSignedAccelerated) (bool32ToBool integerDotProduct8BitMixedSignednessAccelerated) (bool32ToBool integerDotProduct4x8BitPackedUnsignedAccelerated) (bool32ToBool integerDotProduct4x8BitPackedSignedAccelerated) (bool32ToBool integerDotProduct4x8BitPackedMixedSignednessAccelerated) (bool32ToBool integerDotProduct16BitUnsignedAccelerated) (bool32ToBool integerDotProduct16BitSignedAccelerated) (bool32ToBool integerDotProduct16BitMixedSignednessAccelerated) (bool32ToBool integerDotProduct32BitUnsignedAccelerated) (bool32ToBool integerDotProduct32BitSignedAccelerated) (bool32ToBool integerDotProduct32BitMixedSignednessAccelerated) (bool32ToBool integerDotProduct64BitUnsignedAccelerated) (bool32ToBool integerDotProduct64BitSignedAccelerated) (bool32ToBool integerDotProduct64BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating8BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating8BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating8BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating4x8BitPackedUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating4x8BitPackedSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating4x8BitPackedMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating16BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating16BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating16BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating32BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating32BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating32BitMixedSignednessAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating64BitUnsignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating64BitSignedAccelerated) (bool32ToBool integerDotProductAccumulatingSaturating64BitMixedSignednessAccelerated) storageTexelBufferOffsetAlignmentBytes (bool32ToBool storageTexelBufferOffsetSingleTexelAlignment) uniformTexelBufferOffsetAlignmentBytes (bool32ToBool uniformTexelBufferOffsetSingleTexelAlignment) maxBufferSize

instance Storable PhysicalDeviceVulkan13Properties where
  sizeOf ~_ = 216
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkan13Properties where
  zero = PhysicalDeviceVulkan13Properties
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero

