{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}

module Graphics.Vulkan where

import Data.Bits (Bits, FiniteBits)
import Data.Int (Int32)
import Data.Vector.Fixed.Storable (Vec)
import Data.Vector.Fixed.Cont (ToPeano)
import Data.Void (Void)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CChar, CFloat(..), CSize(..))
import Foreign.Ptr (Ptr, FunPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))


-- * Constants


pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
type VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

pattern VK_UUID_SIZE = 16
type VK_UUID_SIZE = 16

pattern VK_MAX_EXTENSION_NAME_SIZE = 256
type VK_MAX_EXTENSION_NAME_SIZE = 256

pattern VK_MAX_DESCRIPTION_SIZE = 256
type VK_MAX_DESCRIPTION_SIZE = 256

pattern VK_MAX_MEMORY_TYPES = 32
type VK_MAX_MEMORY_TYPES = 32

pattern VK_MAX_MEMORY_HEAPS = 16
type VK_MAX_MEMORY_HEAPS = 16

pattern VK_LOD_CLAMP_NONE = 1000.0

pattern VK_REMAINING_MIP_LEVELS = 0xffffffff :: Word32

pattern VK_REMAINING_ARRAY_LAYERS = 0xffffffff :: Word32

pattern VK_WHOLE_SIZE = 0xffffffffffffffff :: Word64

pattern VK_ATTACHMENT_UNUSED = 0xffffffff :: Word32

pattern VK_TRUE = 1
type VK_TRUE = 1

pattern VK_FALSE = 0
type VK_FALSE = 0

pattern VK_QUEUE_FAMILY_IGNORED = 0xffffffff :: Word32

pattern VK_SUBPASS_EXTERNAL = 0xffffffff :: Word32
-- * Base Types

newtype VkSampleMask = VkSampleMask Word32
  deriving (Eq, Storable)

newtype VkBool32 = VkBool32 Word32
  deriving (Eq, Storable)

type VkFlags = Word32

newtype VkDeviceSize = VkDeviceSize Word64
  deriving (Eq, Storable)

-- * Handle Types

data VkInstance_T
type VkInstance = Ptr VkInstance_T

data VkPhysicalDevice_T
type VkPhysicalDevice = Ptr VkPhysicalDevice_T

data VkDevice_T
type VkDevice = Ptr VkDevice_T

data VkQueue_T
type VkQueue = Ptr VkQueue_T

data VkCommandBuffer_T
type VkCommandBuffer = Ptr VkCommandBuffer_T

newtype VkDeviceMemory = VkDeviceMemory Word64
  deriving (Eq, Storable)

newtype VkCommandPool = VkCommandPool Word64
  deriving (Eq, Storable)

newtype VkBuffer = VkBuffer Word64
  deriving (Eq, Storable)

newtype VkBufferView = VkBufferView Word64
  deriving (Eq, Storable)

newtype VkImage = VkImage Word64
  deriving (Eq, Storable)

newtype VkImageView = VkImageView Word64
  deriving (Eq, Storable)

newtype VkShaderModule = VkShaderModule Word64
  deriving (Eq, Storable)

newtype VkPipeline = VkPipeline Word64
  deriving (Eq, Storable)

newtype VkPipelineLayout = VkPipelineLayout Word64
  deriving (Eq, Storable)

newtype VkSampler = VkSampler Word64
  deriving (Eq, Storable)

newtype VkDescriptorSet = VkDescriptorSet Word64
  deriving (Eq, Storable)

newtype VkDescriptorSetLayout = VkDescriptorSetLayout Word64
  deriving (Eq, Storable)

newtype VkDescriptorPool = VkDescriptorPool Word64
  deriving (Eq, Storable)

newtype VkFence = VkFence Word64
  deriving (Eq, Storable)

newtype VkSemaphore = VkSemaphore Word64
  deriving (Eq, Storable)

newtype VkEvent = VkEvent Word64
  deriving (Eq, Storable)

newtype VkQueryPool = VkQueryPool Word64
  deriving (Eq, Storable)

newtype VkFramebuffer = VkFramebuffer Word64
  deriving (Eq, Storable)

newtype VkRenderPass = VkRenderPass Word64
  deriving (Eq, Storable)

newtype VkPipelineCache = VkPipelineCache Word64
  deriving (Eq, Storable)

newtype VkDisplayKHR = VkDisplayKHR Word64
  deriving (Eq, Storable)

newtype VkDisplayModeKHR = VkDisplayModeKHR Word64
  deriving (Eq, Storable)

newtype VkSurfaceKHR = VkSurfaceKHR Word64
  deriving (Eq, Storable)

newtype VkSwapchainKHR = VkSwapchainKHR Word64
  deriving (Eq, Storable)

newtype VkDebugReportCallbackEXT = VkDebugReportCallbackEXT Word64
  deriving (Eq, Storable)

-- * FuncPointer Types

type PFN_vkInternalAllocationNotification = FunPtr
  (Ptr Void ->
     CSize ->
       VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

type PFN_vkInternalFreeNotification = FunPtr
  (Ptr Void ->
     CSize ->
       VkInternalAllocationType -> VkSystemAllocationScope -> IO ())

type PFN_vkReallocationFunction = FunPtr
  (Ptr Void ->
     Ptr Void ->
       CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))

type PFN_vkAllocationFunction = FunPtr
  (Ptr Void ->
     CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr Void))

type PFN_vkFreeFunction = FunPtr (Ptr Void -> Ptr Void -> IO ())

type PFN_vkVoidFunction = FunPtr (IO ())

type PFN_vkDebugReportCallbackEXT = FunPtr
  (VkDebugReportFlagsEXT ->
     VkDebugReportObjectTypeEXT ->
       Word64 ->
         CSize ->
           Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO VkBool32)

-- * Flags

-- ** VkFramebufferCreateFlags
-- | Opaque flag
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkQueryPoolCreateFlags
-- | Opaque flag
newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkRenderPassCreateFlags
-- | Opaque flag
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkSamplerCreateFlags
-- | Opaque flag
newtype VkSamplerCreateFlags = VkSamplerCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineLayoutCreateFlags
-- | Opaque flag
newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineCacheCreateFlags
-- | Opaque flag
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineDepthStencilStateCreateFlags
-- | Opaque flag
newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineDynamicStateCreateFlags
-- | Opaque flag
newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineColorBlendStateCreateFlags
-- | Opaque flag
newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineMultisampleStateCreateFlags
-- | Opaque flag
newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineRasterizationStateCreateFlags
-- | Opaque flag
newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineViewportStateCreateFlags
-- | Opaque flag
newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineTessellationStateCreateFlags
-- | Opaque flag
newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineInputAssemblyStateCreateFlags
-- | Opaque flag
newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineVertexInputStateCreateFlags
-- | Opaque flag
newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineShaderStageCreateFlags
-- | Opaque flag
newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkDescriptorSetLayoutCreateFlags
-- | Opaque flag
newtype VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkBufferViewCreateFlags
-- | Opaque flag
newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkInstanceCreateFlags
-- | Opaque flag
newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkDeviceCreateFlags
-- | Opaque flag
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkDeviceQueueCreateFlags
-- | Opaque flag
newtype VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkQueueFlags

newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueueFlagBits
type VkQueueFlags = VkQueueFlagBits
-- | Queue supports graphics operations
pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 0x1
-- | Queue supports compute operations
pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 0x2
-- | Queue supports transfer operations
pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 0x4
-- | Queue supports sparse resource memory management operations
pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 0x8


-- ** VkMemoryPropertyFlags

newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkMemoryPropertyFlagBits
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits
-- | If otherwise stated, then allocate memory on device
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 0x1
-- | Memory is mappable by host
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 0x2
-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 0x4
-- | Memory will be cached by the host
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 0x8
-- | Memory may be allocated by the driver when it is required
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 0x10


-- ** VkMemoryHeapFlags

newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkMemoryHeapFlagBits
type VkMemoryHeapFlags = VkMemoryHeapFlagBits
-- | If set, heap represents device memory
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 0x1


-- ** VkAccessFlags

newtype VkAccessFlagBits = VkAccessFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkAccessFlagBits
type VkAccessFlags = VkAccessFlagBits
-- | Controls coherency of indirect command reads
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 0x1
-- | Controls coherency of index reads
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 0x2
-- | Controls coherency of vertex attribute reads
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 0x4
-- | Controls coherency of uniform buffer reads
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 0x8
-- | Controls coherency of input attachment reads
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x10
-- | Controls coherency of shader reads
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 0x20
-- | Controls coherency of shader writes
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 0x40
-- | Controls coherency of color attachment reads
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x80
-- | Controls coherency of color attachment writes
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x100
-- | Controls coherency of depth/stencil attachment reads
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x200
-- | Controls coherency of depth/stencil attachment writes
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x400
-- | Controls coherency of transfer reads
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 0x800
-- | Controls coherency of transfer writes
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 0x1000
-- | Controls coherency of host reads
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 0x2000
-- | Controls coherency of host writes
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 0x4000
-- | Controls coherency of memory reads
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 0x8000
-- | Controls coherency of memory writes
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 0x10000


-- ** VkBufferUsageFlags

newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkBufferUsageFlagBits
type VkBufferUsageFlags = VkBufferUsageFlagBits
-- | Can be used as a source of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 0x1
-- | Can be used as a destination of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 0x2
-- | Can be used as TBO
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x4
-- | Can be used as IBO
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 0x8
-- | Can be used as UBO
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = VkBufferUsageFlagBits 0x10
-- | Can be used as SSBO
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = VkBufferUsageFlagBits 0x20
-- | Can be used as source of fixed-function index fetch (index buffer)
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 0x40
-- | Can be used as source of fixed-function vertex fetch (VBO)
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = VkBufferUsageFlagBits 0x80
-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = VkBufferUsageFlagBits 0x100


-- ** VkBufferCreateFlags

newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkBufferCreateFlagBits
type VkBufferCreateFlags = VkBufferCreateFlagBits
-- | Buffer should support sparse backing
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT = VkBufferCreateFlagBits 0x1
-- | Buffer should support sparse backing with partial residency
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VkBufferCreateFlagBits 0x2
-- | Buffer should support constent data access to physical memory blocks mapped into multiple locations of sparse buffers
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = VkBufferCreateFlagBits 0x4


-- ** VkShaderStageFlags

newtype VkShaderStageFlagBits = VkShaderStageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkShaderStageFlagBits
type VkShaderStageFlags = VkShaderStageFlagBits

pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 0x1

pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = VkShaderStageFlagBits 0x2

pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VkShaderStageFlagBits 0x4

pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 0x8

pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 0x10

pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 0x20

pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 0x1f

pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 0x7fffffff

-- ** VkImageUsageFlags

newtype VkImageUsageFlagBits = VkImageUsageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkImageUsageFlagBits
type VkImageUsageFlags = VkImageUsageFlagBits
-- | Can be used as a source of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 0x1
-- | Can be used as a destination of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 0x2
-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 0x4
-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 0x8
-- | Can be used as framebuffer color attachment
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 0x10
-- | Can be used as framebuffer depth/stencil attachment
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 0x20
-- | Image data not needed outside of rendering
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x40
-- | Can be used as framebuffer input attachment
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x80


-- ** VkImageCreateFlags

newtype VkImageCreateFlagBits = VkImageCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkImageCreateFlagBits
type VkImageCreateFlags = VkImageCreateFlagBits
-- | Image should support sparse backing
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlagBits 0x1
-- | Image should support sparse backing with partial residency
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlagBits 0x2
-- | Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlagBits 0x4
-- | Allows image views to have different format than the base image
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlagBits 0x8
-- | Allows creating image views with cube type from the created image
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlagBits 0x10


-- ** VkImageViewCreateFlags
-- | Opaque flag
newtype VkImageViewCreateFlags = VkImageViewCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineCreateFlags

newtype VkPipelineCreateFlagBits = VkPipelineCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkPipelineCreateFlagBits
type VkPipelineCreateFlags = VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = VkPipelineCreateFlagBits 0x1

pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = VkPipelineCreateFlagBits 0x2

pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT = VkPipelineCreateFlagBits 0x4


-- ** VkColorComponentFlags

newtype VkColorComponentFlagBits = VkColorComponentFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkColorComponentFlagBits
type VkColorComponentFlags = VkColorComponentFlagBits

pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentFlagBits 0x1

pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentFlagBits 0x2

pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentFlagBits 0x4

pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentFlagBits 0x8


-- ** VkFenceCreateFlags

newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkFenceCreateFlagBits
type VkFenceCreateFlags = VkFenceCreateFlagBits

pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x1


-- ** VkSemaphoreCreateFlags
-- | Opaque flag
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkFormatFeatureFlags

newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkFormatFeatureFlagBits
type VkFormatFeatureFlags = VkFormatFeatureFlagBits
-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VkFormatFeatureFlagBits 0x1
-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = VkFormatFeatureFlagBits 0x2
-- | Format supports atomic operations in case it's used for storage images
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VkFormatFeatureFlagBits 0x4
-- | Format can be used for uniform texel buffers (TBOs)
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x8
-- | Format can be used for storage texel buffers (IBOs)
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x10
-- | Format supports atomic operations in case it's used for storage texel buffers
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VkFormatFeatureFlagBits 0x20
-- | Format can be used for vertex buffers (VBOs)
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = VkFormatFeatureFlagBits 0x40
-- | Format can be used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x80
-- | Format supports blending in case it's used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VkFormatFeatureFlagBits 0x100
-- | Format can be used for depth/stencil attachment images
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x200
-- | Format can be used as the source image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = VkFormatFeatureFlagBits 0x400
-- | Format can be used as the destination image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = VkFormatFeatureFlagBits 0x800
-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VkFormatFeatureFlagBits 0x1000


-- ** VkQueryControlFlags

newtype VkQueryControlFlagBits = VkQueryControlFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueryControlFlagBits
type VkQueryControlFlags = VkQueryControlFlagBits
-- | Require precise results to be collected by the query
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 0x1


-- ** VkQueryResultFlags

newtype VkQueryResultFlagBits = VkQueryResultFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueryResultFlagBits
type VkQueryResultFlags = VkQueryResultFlagBits
-- | Results of the queries are written to the destination buffer as 64-bit values
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 0x1
-- | Results of the queries are waited on before proceeding with the result copy
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 0x2
-- | Besides the results of the query, the availability of the results is also written
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlagBits 0x4
-- | Copy the partial results of the query even if the final results aren't available
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 0x8


-- ** VkShaderModuleCreateFlags
-- | Opaque flag
newtype VkShaderModuleCreateFlags = VkShaderModuleCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkEventCreateFlags
-- | Opaque flag
newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
  deriving (Eq, Storable)

-- ** VkCommandPoolCreateFlags

newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCommandPoolCreateFlagBits
type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits
-- | Command buffers have a short lifetime
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlagBits 0x1
-- | Command buffers may release their memory individually
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlagBits 0x2


-- ** VkCommandPoolResetFlags

newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCommandPoolResetFlagBits
type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits
-- | Release resources owned by the pool
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlagBits 0x1


-- ** VkCommandBufferResetFlags

newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCommandBufferResetFlagBits
type VkCommandBufferResetFlags = VkCommandBufferResetFlagBits
-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlagBits 0x1


-- ** VkCommandBufferUsageFlags

newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCommandBufferUsageFlagBits
type VkCommandBufferUsageFlags = VkCommandBufferUsageFlagBits

pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlagBits 0x1

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlagBits 0x2
-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlagBits 0x4


-- ** VkQueryPipelineStatisticFlags

newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkQueryPipelineStatisticFlagBits
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlagBits 0x1
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x2
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x4
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x8
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x10
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x20
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x40
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x80
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlagBits 0x100
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x200
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x400


-- ** VkMemoryMapFlags
-- | Opaque flag
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Storable)

-- ** VkImageAspectFlags

newtype VkImageAspectFlagBits = VkImageAspectFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkImageAspectFlagBits
type VkImageAspectFlags = VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 0x1

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 0x2

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 0x4

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 0x8


-- ** VkSparseMemoryBindFlags

newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkSparseMemoryBindFlagBits
type VkSparseMemoryBindFlags = VkSparseMemoryBindFlagBits
-- | Operation binds resource metadata to memory
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT = VkSparseMemoryBindFlagBits 0x1


-- ** VkSparseImageFormatFlags

newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkSparseImageFormatFlagBits
type VkSparseImageFormatFlags = VkSparseImageFormatFlagBits
-- | Image uses a single miptail region for all array layers
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VkSparseImageFormatFlagBits 0x1
-- | Image requires mip levels to be an exact multiple of the sparse image block size for non-miptail levels.
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VkSparseImageFormatFlagBits 0x2
-- | Image uses a non-standard sparse block size
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VkSparseImageFormatFlagBits 0x4


-- ** VkSubpassDescriptionFlags
-- | Opaque flag
newtype VkSubpassDescriptionFlags = VkSubpassDescriptionFlags VkFlags
  deriving (Eq, Storable)

-- ** VkPipelineStageFlags

newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkPipelineStageFlagBits
type VkPipelineStageFlags = VkPipelineStageFlagBits
-- | Before subsequent commands are processed
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = VkPipelineStageFlagBits 0x1
-- | Draw/DispatchIndirect command fetch
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = VkPipelineStageFlagBits 0x2
-- | Vertex/index fetch
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = VkPipelineStageFlagBits 0x4
-- | Vertex shading
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = VkPipelineStageFlagBits 0x8
-- | Tessellation control shading
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VkPipelineStageFlagBits 0x10
-- | Tessellation evaluation shading
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VkPipelineStageFlagBits 0x20
-- | Geometry shading
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VkPipelineStageFlagBits 0x40
-- | Fragment shading
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VkPipelineStageFlagBits 0x80
-- | Early fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x100
-- | Late fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x200
-- | Color attachment writes
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VkPipelineStageFlagBits 0x400
-- | Compute shading
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = VkPipelineStageFlagBits 0x800
-- | Transfer/copy operations
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = VkPipelineStageFlagBits 0x1000
-- | After previous commands have completed
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VkPipelineStageFlagBits 0x2000
-- | Indicates host (CPU) is a source/sink of the dependency
pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 0x4000
-- | All stages of the graphics pipeline
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = VkPipelineStageFlagBits 0x8000
-- | All stages supported on the queue
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = VkPipelineStageFlagBits 0x10000


-- ** VkSampleCountFlags

newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkSampleCountFlagBits
type VkSampleCountFlags = VkSampleCountFlagBits
-- | Sample count 1 supported
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x1
-- | Sample count 2 supported
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x2
-- | Sample count 4 supported
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x4
-- | Sample count 8 supported
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x8
-- | Sample count 16 supported
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x10
-- | Sample count 32 supported
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x20
-- | Sample count 64 supported
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x40


-- ** VkAttachmentDescriptionFlags

newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkAttachmentDescriptionFlagBits
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits
-- | The attachment may alias physical memory of another attachment in the same render pass
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 0x1


-- ** VkStencilFaceFlags

newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkStencilFaceFlagBits
type VkStencilFaceFlags = VkStencilFaceFlagBits
-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 0x1
-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 0x2
-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x3

-- ** VkCullModeFlags

newtype VkCullModeFlagBits = VkCullModeFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCullModeFlagBits
type VkCullModeFlags = VkCullModeFlagBits

pattern VK_CULL_MODE_FRONT_BIT = VkCullModeFlagBits 0x1

pattern VK_CULL_MODE_BACK_BIT = VkCullModeFlagBits 0x2

pattern VK_CULL_MODE_NONE = VkCullModeFlagBits 0x0

pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeFlagBits 0x3

-- ** VkDescriptorPoolCreateFlags

newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkDescriptorPoolCreateFlagBits
type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits
-- | Descriptor sets may be freed individually
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VkDescriptorPoolCreateFlagBits 0x1


-- ** VkDescriptorPoolResetFlags
-- | Opaque flag
newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
  deriving (Eq, Storable)

-- ** VkDependencyFlags

newtype VkDependencyFlagBits = VkDependencyFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkDependencyFlagBits
type VkDependencyFlags = VkDependencyFlagBits
-- | Dependency is per pixel region 
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 0x1


-- ** VkCompositeAlphaFlagsKHR

newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkCompositeAlphaFlagBitsKHR
type VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagBitsKHR

pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x1

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x2

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x4

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x8


-- ** VkDisplayPlaneAlphaFlagsKHR

newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkDisplayPlaneAlphaFlagBitsKHR
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR

pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x1

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x2

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x4

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x8


-- ** VkSurfaceTransformFlagsKHR

newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkSurfaceTransformFlagBitsKHR
type VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagBitsKHR

pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x1

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x2

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x4

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x8

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x10

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x20

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x40

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x80

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x100


-- ** VkSwapchainCreateFlagsKHR
-- | Opaque flag
newtype VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkDisplayModeCreateFlagsKHR
-- | Opaque flag
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkDisplaySurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkAndroidSurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkAndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkMirSurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkMirSurfaceCreateFlagsKHR = VkMirSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkWaylandSurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkWaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkWin32SurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkWin32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkXlibSurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkXlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkXcbSurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkXcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

-- ** VkDebugReportFlagsEXT

newtype VkDebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkDebugReportFlagBitsEXT
type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT

pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT = VkDebugReportFlagBitsEXT 0x1

pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x2

pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 0x4

pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 0x8

pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 0x10


-- * Enumerations

-- ** VkImageLayout

newtype VkImageLayout = VkImageLayout Int32
  deriving (Eq, Storable)
-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0
-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1
-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2
-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 3
-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 4
-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5
-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6
-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7
-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8

-- ** VkAttachmentLoadOp

newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
  deriving (Eq, Storable)

pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2

-- ** VkAttachmentStoreOp

newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
  deriving (Eq, Storable)

pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1

-- ** VkImageType

newtype VkImageType = VkImageType Int32
  deriving (Eq, Storable)

pattern VK_IMAGE_TYPE_1D = VkImageType 0

pattern VK_IMAGE_TYPE_2D = VkImageType 1

pattern VK_IMAGE_TYPE_3D = VkImageType 2

-- ** VkImageTiling

newtype VkImageTiling = VkImageTiling Int32
  deriving (Eq, Storable)

pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- ** VkImageViewType

newtype VkImageViewType = VkImageViewType Int32
  deriving (Eq, Storable)

pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6

-- ** VkCommandBufferLevel

newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
  deriving (Eq, Storable)

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

-- ** VkComponentSwizzle

newtype VkComponentSwizzle = VkComponentSwizzle Int32
  deriving (Eq, Storable)

pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0

pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1

pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2

pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3

pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4

pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5

pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6

-- ** VkDescriptorType

newtype VkDescriptorType = VkDescriptorType Int32
  deriving (Eq, Storable)

pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VkDescriptorType 1

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VkDescriptorType 4

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VkDescriptorType 5

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VkDescriptorType 8

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VkDescriptorType 9

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10

-- ** VkQueryType

newtype VkQueryType = VkQueryType Int32
  deriving (Eq, Storable)

pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0
-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2

-- ** VkBorderColor

newtype VkBorderColor = VkBorderColor Int32
  deriving (Eq, Storable)

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0

pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2

pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3

pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4

pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5

-- ** VkPipelineBindPoint

newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
  deriving (Eq, Storable)

pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

-- ** VkPipelineCacheHeaderVersion

newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int32
  deriving (Eq, Storable)

pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE = VkPipelineCacheHeaderVersion 1

-- ** VkPrimitiveTopology

newtype VkPrimitiveTopology = VkPrimitiveTopology Int32
  deriving (Eq, Storable)

pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = VkPrimitiveTopology 0

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = VkPrimitiveTopology 1

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = VkPrimitiveTopology 2

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VkPrimitiveTopology 3

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = VkPrimitiveTopology 4

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VkPrimitiveTopology 5

pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 6

pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 7

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 8

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 9

pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = VkPrimitiveTopology 10

-- ** VkSharingMode

newtype VkSharingMode = VkSharingMode Int32
  deriving (Eq, Storable)

pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1

-- ** VkIndexType

newtype VkIndexType = VkIndexType Int32
  deriving (Eq, Storable)

pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1

-- ** VkFilter

newtype VkFilter = VkFilter Int32
  deriving (Eq, Storable)

pattern VK_FILTER_NEAREST = VkFilter 0

pattern VK_FILTER_LINEAR = VkFilter 1

-- ** VkSamplerMipmapMode

newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int32
  deriving (Eq, Storable)
-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0
-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1

-- ** VkSamplerAddressMode

newtype VkSamplerAddressMode = VkSamplerAddressMode Int32
  deriving (Eq, Storable)

pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0

pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3

pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = VkSamplerAddressMode 4

-- ** VkCompareOp

newtype VkCompareOp = VkCompareOp Int32
  deriving (Eq, Storable)

pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

pattern VK_COMPARE_OP_LESS = VkCompareOp 1

pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7

-- ** VkPolygonMode

newtype VkPolygonMode = VkPolygonMode Int32
  deriving (Eq, Storable)

pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0

pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1

pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2

-- ** VkFrontFace

newtype VkFrontFace = VkFrontFace Int32
  deriving (Eq, Storable)

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0

pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1

-- ** VkBlendFactor

newtype VkBlendFactor = VkBlendFactor Int32
  deriving (Eq, Storable)

pattern VK_BLEND_FACTOR_ZERO = VkBlendFactor 0

pattern VK_BLEND_FACTOR_ONE = VkBlendFactor 1

pattern VK_BLEND_FACTOR_SRC_COLOR = VkBlendFactor 2

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VkBlendFactor 3

pattern VK_BLEND_FACTOR_DST_COLOR = VkBlendFactor 4

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = VkBlendFactor 5

pattern VK_BLEND_FACTOR_SRC_ALPHA = VkBlendFactor 6

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VkBlendFactor 7

pattern VK_BLEND_FACTOR_DST_ALPHA = VkBlendFactor 8

pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VkBlendFactor 9

pattern VK_BLEND_FACTOR_CONSTANT_COLOR = VkBlendFactor 10

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VkBlendFactor 11

pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = VkBlendFactor 12

pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VkBlendFactor 13

pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = VkBlendFactor 14

pattern VK_BLEND_FACTOR_SRC1_COLOR = VkBlendFactor 15

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VkBlendFactor 16

pattern VK_BLEND_FACTOR_SRC1_ALPHA = VkBlendFactor 17

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VkBlendFactor 18

-- ** VkBlendOp

newtype VkBlendOp = VkBlendOp Int32
  deriving (Eq, Storable)

pattern VK_BLEND_OP_ADD = VkBlendOp 0

pattern VK_BLEND_OP_SUBTRACT = VkBlendOp 1

pattern VK_BLEND_OP_REVERSE_SUBTRACT = VkBlendOp 2

pattern VK_BLEND_OP_MIN = VkBlendOp 3

pattern VK_BLEND_OP_MAX = VkBlendOp 4

-- ** VkStencilOp

newtype VkStencilOp = VkStencilOp Int32
  deriving (Eq, Storable)

pattern VK_STENCIL_OP_KEEP = VkStencilOp 0

pattern VK_STENCIL_OP_ZERO = VkStencilOp 1

pattern VK_STENCIL_OP_REPLACE = VkStencilOp 2

pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP = VkStencilOp 3

pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP = VkStencilOp 4

pattern VK_STENCIL_OP_INVERT = VkStencilOp 5

pattern VK_STENCIL_OP_INCREMENT_AND_WRAP = VkStencilOp 6

pattern VK_STENCIL_OP_DECREMENT_AND_WRAP = VkStencilOp 7

-- ** VkLogicOp

newtype VkLogicOp = VkLogicOp Int32
  deriving (Eq, Storable)

pattern VK_LOGIC_OP_CLEAR = VkLogicOp 0

pattern VK_LOGIC_OP_AND = VkLogicOp 1

pattern VK_LOGIC_OP_AND_REVERSE = VkLogicOp 2

pattern VK_LOGIC_OP_COPY = VkLogicOp 3

pattern VK_LOGIC_OP_AND_INVERTED = VkLogicOp 4

pattern VK_LOGIC_OP_NO_OP = VkLogicOp 5

pattern VK_LOGIC_OP_XOR = VkLogicOp 6

pattern VK_LOGIC_OP_OR = VkLogicOp 7

pattern VK_LOGIC_OP_NOR = VkLogicOp 8

pattern VK_LOGIC_OP_EQUIVALENT = VkLogicOp 9

pattern VK_LOGIC_OP_INVERT = VkLogicOp 10

pattern VK_LOGIC_OP_OR_REVERSE = VkLogicOp 11

pattern VK_LOGIC_OP_COPY_INVERTED = VkLogicOp 12

pattern VK_LOGIC_OP_OR_INVERTED = VkLogicOp 13

pattern VK_LOGIC_OP_NAND = VkLogicOp 14

pattern VK_LOGIC_OP_SET = VkLogicOp 15

-- ** VkInternalAllocationType

newtype VkInternalAllocationType = VkInternalAllocationType Int32
  deriving (Eq, Storable)

pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0

-- ** VkSystemAllocationScope

newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
  deriving (Eq, Storable)

pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = VkSystemAllocationScope 0

pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = VkSystemAllocationScope 1

pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = VkSystemAllocationScope 2

pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = VkSystemAllocationScope 3

pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = VkSystemAllocationScope 4

-- ** VkPhysicalDeviceType

newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
  deriving (Eq, Storable)

pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VkPhysicalDeviceType 1

pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VkPhysicalDeviceType 2

pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VkPhysicalDeviceType 3

pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4

-- ** VkVertexInputRate

newtype VkVertexInputRate = VkVertexInputRate Int32
  deriving (Eq, Storable)

pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1

-- ** VkFormat
-- | Vulkan format definitions
newtype VkFormat = VkFormat Int32
  deriving (Eq, Storable)

pattern VK_FORMAT_UNDEFINED = VkFormat 0

pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8

pattern VK_FORMAT_R8_UNORM = VkFormat 9

pattern VK_FORMAT_R8_SNORM = VkFormat 10

pattern VK_FORMAT_R8_USCALED = VkFormat 11

pattern VK_FORMAT_R8_SSCALED = VkFormat 12

pattern VK_FORMAT_R8_UINT = VkFormat 13

pattern VK_FORMAT_R8_SINT = VkFormat 14

pattern VK_FORMAT_R8_SRGB = VkFormat 15

pattern VK_FORMAT_R8G8_UNORM = VkFormat 16

pattern VK_FORMAT_R8G8_SNORM = VkFormat 17

pattern VK_FORMAT_R8G8_USCALED = VkFormat 18

pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19

pattern VK_FORMAT_R8G8_UINT = VkFormat 20

pattern VK_FORMAT_R8G8_SINT = VkFormat 21

pattern VK_FORMAT_R8G8_SRGB = VkFormat 22

pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23

pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24

pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25

pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26

pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27

pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28

pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29

pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30

pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31

pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32

pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33

pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34

pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35

pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36

pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37

pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38

pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39

pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40

pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41

pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42

pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43

pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44

pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45

pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46

pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47

pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48

pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49

pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69

pattern VK_FORMAT_R16_UNORM = VkFormat 70

pattern VK_FORMAT_R16_SNORM = VkFormat 71

pattern VK_FORMAT_R16_USCALED = VkFormat 72

pattern VK_FORMAT_R16_SSCALED = VkFormat 73

pattern VK_FORMAT_R16_UINT = VkFormat 74

pattern VK_FORMAT_R16_SINT = VkFormat 75

pattern VK_FORMAT_R16_SFLOAT = VkFormat 76

pattern VK_FORMAT_R16G16_UNORM = VkFormat 77

pattern VK_FORMAT_R16G16_SNORM = VkFormat 78

pattern VK_FORMAT_R16G16_USCALED = VkFormat 79

pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80

pattern VK_FORMAT_R16G16_UINT = VkFormat 81

pattern VK_FORMAT_R16G16_SINT = VkFormat 82

pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83

pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84

pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85

pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86

pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87

pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88

pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89

pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90

pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91

pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92

pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93

pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94

pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95

pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96

pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97

pattern VK_FORMAT_R32_UINT = VkFormat 98

pattern VK_FORMAT_R32_SINT = VkFormat 99

pattern VK_FORMAT_R32_SFLOAT = VkFormat 100

pattern VK_FORMAT_R32G32_UINT = VkFormat 101

pattern VK_FORMAT_R32G32_SINT = VkFormat 102

pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103

pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104

pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105

pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106

pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107

pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108

pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109

pattern VK_FORMAT_R64_UINT = VkFormat 110

pattern VK_FORMAT_R64_SINT = VkFormat 111

pattern VK_FORMAT_R64_SFLOAT = VkFormat 112

pattern VK_FORMAT_R64G64_UINT = VkFormat 113

pattern VK_FORMAT_R64G64_SINT = VkFormat 114

pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115

pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116

pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117

pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118

pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119

pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120

pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123

pattern VK_FORMAT_D16_UNORM = VkFormat 124

pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125

pattern VK_FORMAT_D32_SFLOAT = VkFormat 126

pattern VK_FORMAT_S8_UINT = VkFormat 127

pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128

pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129

pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134

pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135

pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136

pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137

pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138

pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139

pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140

pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141

pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144

pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145

pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184

-- ** VkStructureType
-- | Structure type enumerant
newtype VkStructureType = VkStructureType Int32
  deriving (Eq, Storable)

pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = VkStructureType 0

pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VkStructureType 1

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = VkStructureType 2

pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = VkStructureType 3

pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = VkStructureType 4

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VkStructureType 5

pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VkStructureType 6

pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = VkStructureType 7

pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = VkStructureType 8

pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VkStructureType 9

pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = VkStructureType 10

pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = VkStructureType 11

pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = VkStructureType 12

pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = VkStructureType 13

pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = VkStructureType 14

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = VkStructureType 15

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = VkStructureType 16

pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = VkStructureType 17

pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = VkStructureType 18

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = VkStructureType 19

pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = VkStructureType 20

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = VkStructureType 21

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = VkStructureType 22

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = VkStructureType 23

pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = VkStructureType 24

pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = VkStructureType 25

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = VkStructureType 26

pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = VkStructureType 27

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = VkStructureType 28

pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = VkStructureType 29

pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = VkStructureType 30

pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VkStructureType 31

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = VkStructureType 32

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = VkStructureType 33

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = VkStructureType 34

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VkStructureType 35

pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VkStructureType 36

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = VkStructureType 37

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = VkStructureType 38

pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = VkStructureType 39

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = VkStructureType 40

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = VkStructureType 41

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 42

pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = VkStructureType 43

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = VkStructureType 44

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VkStructureType 45

pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = VkStructureType 46

pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = VkStructureType 47

pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = VkStructureType 48

-- ** VkSubpassContents

newtype VkSubpassContents = VkSubpassContents Int32
  deriving (Eq, Storable)

pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VkSubpassContents 1

-- ** VkResult
-- | Error and return codes
newtype VkResult = VkResult Int32
  deriving (Eq, Storable)
-- | Command completed successfully
pattern VK_SUCCESS = VkResult 0
-- | A fence or query has not yet completed
pattern VK_NOT_READY = VkResult 1
-- | A wait operation has not completed in the specified time
pattern VK_TIMEOUT = VkResult 2
-- | An event is signaled
pattern VK_EVENT_SET = VkResult 3
-- | An event is unsignalled
pattern VK_EVENT_RESET = VkResult 4
-- | A return array was too small for the resul
pattern VK_INCOMPLETE = VkResult 5
-- | A host memory allocation has failed
pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)
-- | A device memory allocation has failed
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)
-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)
-- | Initialization of a object has failed
pattern VK_ERROR_DEVICE_LOST = VkResult (-4)
-- | Mapping of a memory object has failed
pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)
-- | Layer specified does not exist
pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)
-- | Extension specified does not exist
pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)
-- | Requested feature is not available on this device
pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)
-- | Unable to find a Vulkan driver
pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)
-- | Too many objects of the type have already been created
pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)
-- | Requested format is not supported on this device
pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)

-- ** VkDynamicState

newtype VkDynamicState = VkDynamicState Int32
  deriving (Eq, Storable)

pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0

pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1

pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2

pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3

pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5

pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6

pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7

pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8

-- ** VkPresentModeKHR

newtype VkPresentModeKHR = VkPresentModeKHR Int32
  deriving (Eq, Storable)

pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3

-- ** VkColorSpaceKHR

newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
  deriving (Eq, Storable)

pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0

-- ** VkDebugReportObjectTypeEXT

newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT Int32
  deriving (Eq, Storable)

pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = VkDebugReportObjectTypeEXT 0

pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = VkDebugReportObjectTypeEXT 1

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = VkDebugReportObjectTypeEXT 2

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = VkDebugReportObjectTypeEXT 3

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = VkDebugReportObjectTypeEXT 4

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = VkDebugReportObjectTypeEXT 5

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = VkDebugReportObjectTypeEXT 6

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = VkDebugReportObjectTypeEXT 7

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = VkDebugReportObjectTypeEXT 8

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = VkDebugReportObjectTypeEXT 9

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = VkDebugReportObjectTypeEXT 10

pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = VkDebugReportObjectTypeEXT 11

pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = VkDebugReportObjectTypeEXT 12

pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = VkDebugReportObjectTypeEXT 13

pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = VkDebugReportObjectTypeEXT 14

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = VkDebugReportObjectTypeEXT 15

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = VkDebugReportObjectTypeEXT 16

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = VkDebugReportObjectTypeEXT 17

pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = VkDebugReportObjectTypeEXT 18

pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = VkDebugReportObjectTypeEXT 19

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = VkDebugReportObjectTypeEXT 20

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = VkDebugReportObjectTypeEXT 21

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = VkDebugReportObjectTypeEXT 22

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = VkDebugReportObjectTypeEXT 23

pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = VkDebugReportObjectTypeEXT 24

pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = VkDebugReportObjectTypeEXT 25

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = VkDebugReportObjectTypeEXT 26

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = VkDebugReportObjectTypeEXT 27

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = VkDebugReportObjectTypeEXT 28

-- ** VkDebugReportErrorEXT

newtype VkDebugReportErrorEXT = VkDebugReportErrorEXT Int32
  deriving (Eq, Storable)

pattern VK_DEBUG_REPORT_ERROR_NONE_EXT = VkDebugReportErrorEXT 0

pattern VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT = VkDebugReportErrorEXT 1

-- * Struct Types


data VkOffset2D =
  VkOffset2D{ vkX :: Int32 
            , vkY :: Int32 
            }
  deriving (Eq)

instance Storable VkOffset2D where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkOffset2D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkOffset2D))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkOffset2D))



data VkOffset3D =
  VkOffset3D{ vkX :: Int32 
            , vkY :: Int32 
            , vkZ :: Int32 
            }
  deriving (Eq)

instance Storable VkOffset3D where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkOffset3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkOffset3D))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkOffset3D))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkOffset3D))



data VkExtent2D =
  VkExtent2D{ vkWidth :: Word32 
            , vkHeight :: Word32 
            }
  deriving (Eq)

instance Storable VkExtent2D where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkExtent2D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkWidth (poked :: VkExtent2D))
                *> poke (ptr `plusPtr` 4) (vkHeight (poked :: VkExtent2D))



data VkExtent3D =
  VkExtent3D{ vkWidth :: Word32 
            , vkHeight :: Word32 
            , vkDepth :: Word32 
            }
  deriving (Eq)

instance Storable VkExtent3D where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkExtent3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkWidth (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 4) (vkHeight (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 8) (vkDepth (poked :: VkExtent3D))



data VkViewport =
  VkViewport{ vkX :: CFloat 
            , vkY :: CFloat 
            , vkWidth :: CFloat 
            , vkHeight :: CFloat 
            , vkMinDepth :: CFloat 
            , vkMaxDepth :: CFloat 
            }
  deriving (Eq)

instance Storable VkViewport where
  sizeOf _ = 24
  alignment _ = 4
  peek ptr = VkViewport <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
                        <*> peek (ptr `plusPtr` 12)
                        <*> peek (ptr `plusPtr` 16)
                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkViewport))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkViewport))
                *> poke (ptr `plusPtr` 8) (vkWidth (poked :: VkViewport))
                *> poke (ptr `plusPtr` 12) (vkHeight (poked :: VkViewport))
                *> poke (ptr `plusPtr` 16) (vkMinDepth (poked :: VkViewport))
                *> poke (ptr `plusPtr` 20) (vkMaxDepth (poked :: VkViewport))



data VkRect2D =
  VkRect2D{ vkOffset :: VkOffset2D 
          , vkExtent :: VkExtent2D 
          }
  deriving (Eq)

instance Storable VkRect2D where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = VkRect2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkRect2D))
                *> poke (ptr `plusPtr` 8) (vkExtent (poked :: VkRect2D))



data VkRect3D =
  VkRect3D{ vkOffset :: VkOffset3D 
          , vkExtent :: VkExtent3D 
          }
  deriving (Eq)

instance Storable VkRect3D where
  sizeOf _ = 24
  alignment _ = 4
  peek ptr = VkRect3D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkRect3D))
                *> poke (ptr `plusPtr` 12) (vkExtent (poked :: VkRect3D))



data VkClearRect =
  VkClearRect{ vkRect :: VkRect2D 
             , vkBaseArrayLayer :: Word32 
             , vkLayerCount :: Word32 
             }
  deriving (Eq)

instance Storable VkClearRect where
  sizeOf _ = 24
  alignment _ = 4
  peek ptr = VkClearRect <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRect (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 16) (vkBaseArrayLayer (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 20) (vkLayerCount (poked :: VkClearRect))



data VkComponentMapping =
  VkComponentMapping{ vkR :: VkComponentSwizzle 
                    , vkG :: VkComponentSwizzle 
                    , vkB :: VkComponentSwizzle 
                    , vkA :: VkComponentSwizzle 
                    }
  deriving (Eq)

instance Storable VkComponentMapping where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = VkComponentMapping <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkR (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 4) (vkG (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 8) (vkB (poked :: VkComponentMapping))
                *> poke (ptr `plusPtr` 12) (vkA (poked :: VkComponentMapping))



data VkPhysicalDeviceProperties =
  VkPhysicalDeviceProperties{ vkApiVersion :: Word32 
                            , vkDriverVersion :: Word32 
                            , vkVendorID :: Word32 
                            , vkDeviceID :: Word32 
                            , vkDeviceType :: VkPhysicalDeviceType 
                            , vkDeviceName :: Vec (ToPeano VK_MAX_PHYSICAL_DEVICE_NAME_SIZE) CChar 
                            , vkPipelineCacheUUID :: Vec (ToPeano VK_UUID_SIZE) Word8 
                            , vkLimits :: VkPhysicalDeviceLimits 
                            , vkSparseProperties :: VkPhysicalDeviceSparseProperties 
                            }
  deriving (Eq)

instance Storable VkPhysicalDeviceProperties where
  sizeOf _ = 824
  alignment _ = 8
  peek ptr = VkPhysicalDeviceProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 12)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 276)
                                        <*> peek (ptr `plusPtr` 296)
                                        <*> peek (ptr `plusPtr` 800)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkApiVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 4) (vkDriverVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 8) (vkVendorID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 12) (vkDeviceID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 16) (vkDeviceType (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 20) (vkDeviceName (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 276) (vkPipelineCacheUUID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 296) (vkLimits (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 800) (vkSparseProperties (poked :: VkPhysicalDeviceProperties))



data VkExtensionProperties =
  VkExtensionProperties{ vkExtensionName :: Vec (ToPeano VK_MAX_EXTENSION_NAME_SIZE) CChar 
                       , vkSpecVersion :: Word32 
                       }
  deriving (Eq)

instance Storable VkExtensionProperties where
  sizeOf _ = 260
  alignment _ = 4
  peek ptr = VkExtensionProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExtensionName (poked :: VkExtensionProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkExtensionProperties))



data VkLayerProperties =
  VkLayerProperties{ vkLayerName :: Vec (ToPeano VK_MAX_EXTENSION_NAME_SIZE) CChar 
                   , vkSpecVersion :: Word32 
                   , vkImplementationVersion :: Word32 
                   , vkDescription :: Vec (ToPeano VK_MAX_DESCRIPTION_SIZE) CChar 
                   }
  deriving (Eq)

instance Storable VkLayerProperties where
  sizeOf _ = 520
  alignment _ = 4
  peek ptr = VkLayerProperties <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 256)
                               <*> peek (ptr `plusPtr` 260)
                               <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLayerName (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 260) (vkImplementationVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 264) (vkDescription (poked :: VkLayerProperties))



data VkApplicationInfo =
  VkApplicationInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkPApplicationName :: Ptr CChar 
                   , vkApplicationVersion :: Word32 
                   , vkPEngineName :: Ptr CChar 
                   , vkEngineVersion :: Word32 
                   , vkApiVersion :: Word32 
                   }
  deriving (Eq)

instance Storable VkApplicationInfo where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkApplicationInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 16) (vkPApplicationName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 24) (vkApplicationVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 32) (vkPEngineName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 40) (vkEngineVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 44) (vkApiVersion (poked :: VkApplicationInfo))



data VkAllocationCallbacks =
  VkAllocationCallbacks{ vkPUserData :: Ptr Void 
                       , vkPfnAllocation :: PFN_vkAllocationFunction 
                       , vkPfnReallocation :: PFN_vkReallocationFunction 
                       , vkPfnFree :: PFN_vkFreeFunction 
                       , vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification 
                       , vkPfnInternalFree :: PFN_vkInternalFreeNotification 
                       }
  deriving (Eq)

instance Storable VkAllocationCallbacks where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkAllocationCallbacks <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPUserData (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 8) (vkPfnAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 16) (vkPfnReallocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 24) (vkPfnFree (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 32) (vkPfnInternalAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 40) (vkPfnInternalFree (poked :: VkAllocationCallbacks))



data VkDeviceQueueCreateInfo =
  VkDeviceQueueCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkDeviceQueueCreateFlags 
                         , vkQueueFamilyIndex :: Word32 
                         , vkQueueCount :: Word32 
                         , vkPQueuePriorities :: Ptr CFloat 
                         }
  deriving (Eq)

instance Storable VkDeviceQueueCreateInfo where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkDeviceQueueCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueueCount (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPQueuePriorities (poked :: VkDeviceQueueCreateInfo))



data VkDeviceCreateInfo =
  VkDeviceCreateInfo{ vkSType :: VkStructureType 
                    , vkPNext :: Ptr Void 
                    , vkFlags :: VkDeviceCreateFlags 
                    , vkQueueCreateInfoCount :: Word32 
                    , vkPQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo 
                    , vkEnabledLayerCount :: Word32 
                    , vkPpEnabledLayerNames :: Ptr (Ptr CChar) 
                    , vkEnabledExtensionCount :: Word32 
                    , vkPpEnabledExtensionNames :: Ptr (Ptr CChar) 
                    , vkPEnabledFeatures :: Ptr VkPhysicalDeviceFeatures 
                    }
  deriving (Eq)

instance Storable VkDeviceCreateInfo where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr = VkDeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
                                <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueCreateInfoCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPQueueCreateInfos (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPpEnabledLayerNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPpEnabledExtensionNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPEnabledFeatures (poked :: VkDeviceCreateInfo))



data VkInstanceCreateInfo =
  VkInstanceCreateInfo{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkFlags :: VkInstanceCreateFlags 
                      , vkPApplicationInfo :: Ptr VkApplicationInfo 
                      , vkEnabledLayerCount :: Word32 
                      , vkPpEnabledLayerNames :: Ptr (Ptr CChar) 
                      , vkEnabledExtensionCount :: Word32 
                      , vkPpEnabledExtensionNames :: Ptr (Ptr CChar) 
                      }
  deriving (Eq)

instance Storable VkInstanceCreateInfo where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkInstanceCreateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPApplicationInfo (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPpEnabledLayerNames (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPpEnabledExtensionNames (poked :: VkInstanceCreateInfo))



data VkQueueFamilyProperties =
  VkQueueFamilyProperties{ vkQueueFlags :: VkQueueFlags 
                         , vkQueueCount :: Word32 
                         , vkTimestampValidBits :: Word32 
                         , vkMinImageTransferGranularity :: VkExtent3D 
                         }
  deriving (Eq)

instance Storable VkQueueFamilyProperties where
  sizeOf _ = 24
  alignment _ = 4
  peek ptr = VkQueueFamilyProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkQueueFlags (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 4) (vkQueueCount (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 8) (vkTimestampValidBits (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 12) (vkMinImageTransferGranularity (poked :: VkQueueFamilyProperties))



data VkPhysicalDeviceMemoryProperties =
  VkPhysicalDeviceMemoryProperties{ vkMemoryTypeCount :: Word32 
                                  , vkMemoryTypes :: Vec (ToPeano VK_MAX_MEMORY_TYPES) VkMemoryType 
                                  , vkMemoryHeapCount :: Word32 
                                  , vkMemoryHeaps :: Vec (ToPeano VK_MAX_MEMORY_HEAPS) VkMemoryHeap 
                                  }
  deriving (Eq)

instance Storable VkPhysicalDeviceMemoryProperties where
  sizeOf _ = 520
  alignment _ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 260)
                                              <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMemoryTypeCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 4) (vkMemoryTypes (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 260) (vkMemoryHeapCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 264) (vkMemoryHeaps (poked :: VkPhysicalDeviceMemoryProperties))



data VkMemoryAllocateInfo =
  VkMemoryAllocateInfo{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkAllocationSize :: VkDeviceSize 
                      , vkMemoryTypeIndex :: Word32 
                      }
  deriving (Eq)

instance Storable VkMemoryAllocateInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeIndex (poked :: VkMemoryAllocateInfo))



data VkMemoryRequirements =
  VkMemoryRequirements{ vkSize :: VkDeviceSize 
                      , vkAlignment :: VkDeviceSize 
                      , vkMemoryTypeBits :: Word32 
                      }
  deriving (Eq)

instance Storable VkMemoryRequirements where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 8) (vkAlignment (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryRequirements))



data VkSparseImageFormatProperties =
  VkSparseImageFormatProperties{ vkAspectMask :: VkImageAspectFlags 
                               , vkImageGranularity :: VkExtent3D 
                               , vkFlags :: VkSparseImageFormatFlags 
                               }
  deriving (Eq)

instance Storable VkSparseImageFormatProperties where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = VkSparseImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkImageGranularity (poked :: VkSparseImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSparseImageFormatProperties))



data VkSparseImageMemoryRequirements =
  VkSparseImageMemoryRequirements{ vkFormatProperties :: VkSparseImageFormatProperties 
                                 , vkImageMipTailFirstLod :: Word32 
                                 , vkImageMipTailSize :: VkDeviceSize 
                                 , vkImageMipTailOffset :: VkDeviceSize 
                                 , vkImageMipTailStride :: VkDeviceSize 
                                 }
  deriving (Eq)

instance Storable VkSparseImageMemoryRequirements where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkSparseImageMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFormatProperties (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 20) (vkImageMipTailFirstLod (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 24) (vkImageMipTailSize (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 32) (vkImageMipTailOffset (poked :: VkSparseImageMemoryRequirements))
                *> poke (ptr `plusPtr` 40) (vkImageMipTailStride (poked :: VkSparseImageMemoryRequirements))



data VkMemoryType =
  VkMemoryType{ vkPropertyFlags :: VkMemoryPropertyFlags 
              , vkHeapIndex :: Word32 
              }
  deriving (Eq)

instance Storable VkMemoryType where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkMemoryType <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPropertyFlags (poked :: VkMemoryType))
                *> poke (ptr `plusPtr` 4) (vkHeapIndex (poked :: VkMemoryType))



data VkMemoryHeap =
  VkMemoryHeap{ vkSize :: VkDeviceSize 
              , vkFlags :: VkMemoryHeapFlags 
              }
  deriving (Eq)

instance Storable VkMemoryHeap where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = VkMemoryHeap <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryHeap))
                *> poke (ptr `plusPtr` 8) (vkFlags (poked :: VkMemoryHeap))



data VkMappedMemoryRange =
  VkMappedMemoryRange{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkMemory :: VkDeviceMemory 
                     , vkOffset :: VkDeviceSize 
                     , vkSize :: VkDeviceSize 
                     }
  deriving (Eq)

instance Storable VkMappedMemoryRange where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkMappedMemoryRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (vkSize (poked :: VkMappedMemoryRange))



data VkFormatProperties =
  VkFormatProperties{ vkLinearTilingFeatures :: VkFormatFeatureFlags 
                    , vkOptimalTilingFeatures :: VkFormatFeatureFlags 
                    , vkBufferFeatures :: VkFormatFeatureFlags 
                    }
  deriving (Eq)

instance Storable VkFormatProperties where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkFormatProperties <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLinearTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkOptimalTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkBufferFeatures (poked :: VkFormatProperties))



data VkImageFormatProperties =
  VkImageFormatProperties{ vkMaxExtent :: VkExtent3D 
                         , vkMaxMipLevels :: Word32 
                         , vkMaxArrayLayers :: Word32 
                         , vkSampleCounts :: VkSampleCountFlags 
                         , vkMaxResourceSize :: VkDeviceSize 
                         }
  deriving (Eq)

instance Storable VkImageFormatProperties where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxExtent (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 12) (vkMaxMipLevels (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkMaxArrayLayers (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 20) (vkSampleCounts (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 24) (vkMaxResourceSize (poked :: VkImageFormatProperties))



data VkDescriptorBufferInfo =
  VkDescriptorBufferInfo{ vkBuffer :: VkBuffer 
                        , vkOffset :: VkDeviceSize 
                        , vkRange :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkDescriptorBufferInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkDescriptorBufferInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 8) (vkOffset (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkRange (poked :: VkDescriptorBufferInfo))



data VkDescriptorImageInfo =
  VkDescriptorImageInfo{ vkSampler :: VkSampler 
                       , vkImageView :: VkImageView 
                       , vkImageLayout :: VkImageLayout 
                       }
  deriving (Eq)

instance Storable VkDescriptorImageInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkDescriptorImageInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSampler (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 8) (vkImageView (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 16) (vkImageLayout (poked :: VkDescriptorImageInfo))



data VkWriteDescriptorSet =
  VkWriteDescriptorSet{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkDstSet :: VkDescriptorSet 
                      , vkDstBinding :: Word32 
                      , vkDstArrayElement :: Word32 
                      , vkDescriptorCount :: Word32 
                      , vkDescriptorType :: VkDescriptorType 
                      , vkPImageInfo :: Ptr VkDescriptorImageInfo 
                      , vkPBufferInfo :: Ptr VkDescriptorBufferInfo 
                      , vkPTexelBufferView :: Ptr VkBufferView 
                      }
  deriving (Eq)

instance Storable VkWriteDescriptorSet where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkWriteDescriptorSet <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkDstSet (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkDstBinding (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkDstArrayElement (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDescriptorCount (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 36) (vkDescriptorType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkPImageInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkPBufferInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 56) (vkPTexelBufferView (poked :: VkWriteDescriptorSet))



data VkCopyDescriptorSet =
  VkCopyDescriptorSet{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkSrcSet :: VkDescriptorSet 
                     , vkSrcBinding :: Word32 
                     , vkSrcArrayElement :: Word32 
                     , vkDstSet :: VkDescriptorSet 
                     , vkDstBinding :: Word32 
                     , vkDstArrayElement :: Word32 
                     , vkDescriptorCount :: Word32 
                     }
  deriving (Eq)

instance Storable VkCopyDescriptorSet where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkCopyDescriptorSet <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkSrcSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkSrcBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkSrcArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDstSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkDstBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 44) (vkDstArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkDescriptorCount (poked :: VkCopyDescriptorSet))



data VkBufferCreateInfo =
  VkBufferCreateInfo{ vkSType :: VkStructureType 
                    , vkPNext :: Ptr Void 
                    , vkFlags :: VkBufferCreateFlags 
                    , vkSize :: VkDeviceSize 
                    , vkUsage :: VkBufferUsageFlags 
                    , vkSharingMode :: VkSharingMode 
                    , vkQueueFamilyIndexCount :: Word32 
                    , vkPQueueFamilyIndices :: Ptr Word32 
                    }
  deriving (Eq)

instance Storable VkBufferCreateInfo where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkBufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 36)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSize (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkUsage (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkSharingMode (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkQueueFamilyIndexCount (poked :: VkBufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPQueueFamilyIndices (poked :: VkBufferCreateInfo))



data VkBufferViewCreateInfo =
  VkBufferViewCreateInfo{ vkSType :: VkStructureType 
                        , vkPNext :: Ptr Void 
                        , vkFlags :: VkBufferViewCreateFlags 
                        , vkBuffer :: VkBuffer 
                        , vkFormat :: VkFormat 
                        , vkOffset :: VkDeviceSize 
                        , vkRange :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkBufferViewCreateInfo where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkBufferViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkFormat (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkRange (poked :: VkBufferViewCreateInfo))



data VkImageSubresource =
  VkImageSubresource{ vkAspectMask :: VkImageAspectFlags 
                    , vkMipLevel :: Word32 
                    , vkArrayLayer :: Word32 
                    }
  deriving (Eq)

instance Storable VkImageSubresource where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkImageSubresource <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 4) (vkMipLevel (poked :: VkImageSubresource))
                *> poke (ptr `plusPtr` 8) (vkArrayLayer (poked :: VkImageSubresource))



data VkImageSubresourceLayers =
  VkImageSubresourceLayers{ vkAspectMask :: VkImageAspectFlags 
                          , vkMipLevel :: Word32 
                          , vkBaseArrayLayer :: Word32 
                          , vkLayerCount :: Word32 
                          }
  deriving (Eq)

instance Storable VkImageSubresourceLayers where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = VkImageSubresourceLayers <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 4) (vkMipLevel (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 8) (vkBaseArrayLayer (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 12) (vkLayerCount (poked :: VkImageSubresourceLayers))



data VkImageSubresourceRange =
  VkImageSubresourceRange{ vkAspectMask :: VkImageAspectFlags 
                         , vkBaseMipLevel :: Word32 
                         , vkLevelCount :: Word32 
                         , vkBaseArrayLayer :: Word32 
                         , vkLayerCount :: Word32 
                         }
  deriving (Eq)

instance Storable VkImageSubresourceRange where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = VkImageSubresourceRange <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 4) (vkBaseMipLevel (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 8) (vkLevelCount (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 12) (vkBaseArrayLayer (poked :: VkImageSubresourceRange))
                *> poke (ptr `plusPtr` 16) (vkLayerCount (poked :: VkImageSubresourceRange))



data VkMemoryBarrier =
  VkMemoryBarrier{ vkSType :: VkStructureType 
                 , vkPNext :: Ptr Void 
                 , vkSrcAccessMask :: VkAccessFlags 
                 , vkDstAccessMask :: VkAccessFlags 
                 }
  deriving (Eq)

instance Storable VkMemoryBarrier where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkMemoryBarrier <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkMemoryBarrier))



data VkBufferMemoryBarrier =
  VkBufferMemoryBarrier{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkSrcAccessMask :: VkAccessFlags 
                       , vkDstAccessMask :: VkAccessFlags 
                       , vkSrcQueueFamilyIndex :: Word32 
                       , vkDstQueueFamilyIndex :: Word32 
                       , vkBuffer :: VkBuffer 
                       , vkOffset :: VkDeviceSize 
                       , vkSize :: VkDeviceSize 
                       }
  deriving (Eq)

instance Storable VkBufferMemoryBarrier where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkBufferMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkSrcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkDstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkBuffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSize (poked :: VkBufferMemoryBarrier))



data VkImageMemoryBarrier =
  VkImageMemoryBarrier{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkSrcAccessMask :: VkAccessFlags 
                      , vkDstAccessMask :: VkAccessFlags 
                      , vkOldLayout :: VkImageLayout 
                      , vkNewLayout :: VkImageLayout 
                      , vkSrcQueueFamilyIndex :: Word32 
                      , vkDstQueueFamilyIndex :: Word32 
                      , vkImage :: VkImage 
                      , vkSubresourceRange :: VkImageSubresourceRange 
                      }
  deriving (Eq)

instance Storable VkImageMemoryBarrier where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr = VkImageMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 20)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkOldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkNewLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkSrcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (vkDstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkImage (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSubresourceRange (poked :: VkImageMemoryBarrier))



data VkImageCreateInfo =
  VkImageCreateInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkFlags :: VkImageCreateFlags 
                   , vkImageType :: VkImageType 
                   , vkFormat :: VkFormat 
                   , vkExtent :: VkExtent3D 
                   , vkMipLevels :: Word32 
                   , vkArrayLayers :: Word32 
                   , vkSamples :: VkSampleCountFlagBits 
                   , vkTiling :: VkImageTiling 
                   , vkUsage :: VkImageUsageFlags 
                   , vkSharingMode :: VkSharingMode 
                   , vkQueueFamilyIndexCount :: Word32 
                   , vkPQueueFamilyIndices :: Ptr Word32 
                   , vkInitialLayout :: VkImageLayout 
                   }
  deriving (Eq)

instance Storable VkImageCreateInfo where
  sizeOf _ = 88
  alignment _ = 8
  peek ptr = VkImageCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 20)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 28)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
                               <*> peek (ptr `plusPtr` 48)
                               <*> peek (ptr `plusPtr` 52)
                               <*> peek (ptr `plusPtr` 56)
                               <*> peek (ptr `plusPtr` 60)
                               <*> peek (ptr `plusPtr` 64)
                               <*> peek (ptr `plusPtr` 72)
                               <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkImageType (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkFormat (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkExtent (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkMipLevels (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkArrayLayers (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkSamples (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkTiling (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkUsage (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkSharingMode (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkImageCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkInitialLayout (poked :: VkImageCreateInfo))



data VkSubresourceLayout =
  VkSubresourceLayout{ vkOffset :: VkDeviceSize 
                     , vkSize :: VkDeviceSize 
                     , vkRowPitch :: VkDeviceSize 
                     , vkArrayPitch :: VkDeviceSize 
                     , vkDepthPitch :: VkDeviceSize 
                     }
  deriving (Eq)

instance Storable VkSubresourceLayout where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkSubresourceLayout <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 16) (vkRowPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 24) (vkArrayPitch (poked :: VkSubresourceLayout))
                *> poke (ptr `plusPtr` 32) (vkDepthPitch (poked :: VkSubresourceLayout))



data VkImageViewCreateInfo =
  VkImageViewCreateInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkFlags :: VkImageViewCreateFlags 
                       , vkImage :: VkImage 
                       , vkViewType :: VkImageViewType 
                       , vkFormat :: VkFormat 
                       , vkComponents :: VkComponentMapping 
                       , vkSubresourceRange :: VkImageSubresourceRange 
                       }
  deriving (Eq)

instance Storable VkImageViewCreateInfo where
  sizeOf _ = 80
  alignment _ = 8
  peek ptr = VkImageViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 36)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkImage (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkViewType (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkFormat (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkComponents (poked :: VkImageViewCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkSubresourceRange (poked :: VkImageViewCreateInfo))



data VkBufferCopy =
  VkBufferCopy{ vkSrcOffset :: VkDeviceSize 
              , vkDstOffset :: VkDeviceSize 
              , vkSize :: VkDeviceSize 
              }
  deriving (Eq)

instance Storable VkBufferCopy where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkBufferCopy <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 8) (vkDstOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 16) (vkSize (poked :: VkBufferCopy))



data VkSparseMemoryBind =
  VkSparseMemoryBind{ vkResourceOffset :: VkDeviceSize 
                    , vkSize :: VkDeviceSize 
                    , vkMemory :: VkDeviceMemory 
                    , vkMemoryOffset :: VkDeviceSize 
                    , vkFlags :: VkSparseMemoryBindFlags 
                    }
  deriving (Eq)

instance Storable VkSparseMemoryBind where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkSparseMemoryBind <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkResourceOffset (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 24) (vkMemoryOffset (poked :: VkSparseMemoryBind))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkSparseMemoryBind))



data VkSparseImageMemoryBind =
  VkSparseImageMemoryBind{ vkSubresource :: VkImageSubresource 
                         , vkOffset :: VkOffset3D 
                         , vkExtent :: VkExtent3D 
                         , vkMemory :: VkDeviceMemory 
                         , vkMemoryOffset :: VkDeviceSize 
                         , vkFlags :: VkSparseMemoryBindFlags 
                         }
  deriving (Eq)

instance Storable VkSparseImageMemoryBind where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkSparseImageMemoryBind <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubresource (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 12) (vkOffset (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 24) (vkExtent (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 40) (vkMemory (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 48) (vkMemoryOffset (poked :: VkSparseImageMemoryBind))
                *> poke (ptr `plusPtr` 56) (vkFlags (poked :: VkSparseImageMemoryBind))



data VkSparseBufferMemoryBindInfo =
  VkSparseBufferMemoryBindInfo{ vkBuffer :: VkBuffer 
                              , vkBindCount :: Word32 
                              , vkPBinds :: Ptr VkSparseMemoryBind 
                              }
  deriving (Eq)

instance Storable VkSparseBufferMemoryBindInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkSparseBufferMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseBufferMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseBufferMemoryBindInfo))



data VkSparseImageOpaqueMemoryBindInfo =
  VkSparseImageOpaqueMemoryBindInfo{ vkImage :: VkImage 
                                   , vkBindCount :: Word32 
                                   , vkPBinds :: Ptr VkSparseMemoryBind 
                                   }
  deriving (Eq)

instance Storable VkSparseImageOpaqueMemoryBindInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkSparseImageOpaqueMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImage (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseImageOpaqueMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseImageOpaqueMemoryBindInfo))



data VkSparseImageMemoryBindInfo =
  VkSparseImageMemoryBindInfo{ vkImage :: VkImage 
                             , vkBindCount :: Word32 
                             , vkPBinds :: Ptr VkSparseImageMemoryBind 
                             }
  deriving (Eq)

instance Storable VkSparseImageMemoryBindInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkSparseImageMemoryBindInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImage (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 8) (vkBindCount (poked :: VkSparseImageMemoryBindInfo))
                *> poke (ptr `plusPtr` 16) (vkPBinds (poked :: VkSparseImageMemoryBindInfo))



data VkBindSparseInfo =
  VkBindSparseInfo{ vkSType :: VkStructureType 
                  , vkPNext :: Ptr Void 
                  , vkWaitSemaphoreCount :: Word32 
                  , vkPWaitSemaphores :: Ptr VkSemaphore 
                  , vkBufferBindCount :: Word32 
                  , vkPBufferBinds :: Ptr VkSparseBufferMemoryBindInfo 
                  , vkImageOpaqueBindCount :: Word32 
                  , vkPImageOpaqueBinds :: Ptr VkSparseImageOpaqueMemoryBindInfo 
                  , vkImageBindCount :: Word32 
                  , vkPImageBinds :: Ptr VkSparseImageMemoryBindInfo 
                  , vkSignalSemaphoreCount :: Word32 
                  , vkPSignalSemaphores :: Ptr VkSemaphore 
                  }
  deriving (Eq)

instance Storable VkBindSparseInfo where
  sizeOf _ = 96
  alignment _ = 8
  peek ptr = VkBindSparseInfo <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
                              <*> peek (ptr `plusPtr` 64)
                              <*> peek (ptr `plusPtr` 72)
                              <*> peek (ptr `plusPtr` 80)
                              <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 32) (vkBufferBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 40) (vkPBufferBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 48) (vkImageOpaqueBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 56) (vkPImageOpaqueBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 64) (vkImageBindCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 72) (vkPImageBinds (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 80) (vkSignalSemaphoreCount (poked :: VkBindSparseInfo))
                *> poke (ptr `plusPtr` 88) (vkPSignalSemaphores (poked :: VkBindSparseInfo))



data VkImageCopy =
  VkImageCopy{ vkSrcSubresource :: VkImageSubresourceLayers 
             , vkSrcOffset :: VkOffset3D 
             , vkDstSubresource :: VkImageSubresourceLayers 
             , vkDstOffset :: VkOffset3D 
             , vkExtent :: VkExtent3D 
             }
  deriving (Eq)

instance Storable VkImageCopy where
  sizeOf _ = 68
  alignment _ = 4
  peek ptr = VkImageCopy <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 28)
                         <*> peek (ptr `plusPtr` 44)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageCopy))



data VkImageBlit =
  VkImageBlit{ vkSrcSubresource :: VkImageSubresourceLayers 
             , vkSrcOffsets :: Vec (ToPeano 2) VkOffset3D 
             , vkDstSubresource :: VkImageSubresourceLayers 
             , vkDstOffsets :: Vec (ToPeano 2) VkOffset3D 
             }
  deriving (Eq)

instance Storable VkImageBlit where
  sizeOf _ = 80
  alignment _ = 4
  peek ptr = VkImageBlit <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 40)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 16) (vkSrcOffsets (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 40) (vkDstSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 56) (vkDstOffsets (poked :: VkImageBlit))



data VkBufferImageCopy =
  VkBufferImageCopy{ vkBufferOffset :: VkDeviceSize 
                   , vkBufferRowLength :: Word32 
                   , vkBufferImageHeight :: Word32 
                   , vkImageSubresource :: VkImageSubresourceLayers 
                   , vkImageOffset :: VkOffset3D 
                   , vkImageExtent :: VkExtent3D 
                   }
  deriving (Eq)

instance Storable VkBufferImageCopy where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkBufferImageCopy <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 12)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBufferOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 8) (vkBufferRowLength (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 12) (vkBufferImageHeight (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 16) (vkImageSubresource (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 32) (vkImageOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkBufferImageCopy))



data VkImageResolve =
  VkImageResolve{ vkSrcSubresource :: VkImageSubresourceLayers 
                , vkSrcOffset :: VkOffset3D 
                , vkDstSubresource :: VkImageSubresourceLayers 
                , vkDstOffset :: VkOffset3D 
                , vkExtent :: VkExtent3D 
                }
  deriving (Eq)

instance Storable VkImageResolve where
  sizeOf _ = 68
  alignment _ = 4
  peek ptr = VkImageResolve <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 28)
                            <*> peek (ptr `plusPtr` 44)
                            <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageResolve))



data VkShaderModuleCreateInfo =
  VkShaderModuleCreateInfo{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkShaderModuleCreateFlags 
                          , vkCodeSize :: CSize 
                          , vkPCode :: Ptr Word32 
                          }
  deriving (Eq)

instance Storable VkShaderModuleCreateInfo where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkShaderModuleCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkCodeSize (poked :: VkShaderModuleCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPCode (poked :: VkShaderModuleCreateInfo))



data VkDescriptorSetLayoutBinding =
  VkDescriptorSetLayoutBinding{ vkBinding :: Word32 
                              , vkDescriptorType :: VkDescriptorType 
                              , vkDescriptorCount :: Word32 
                              , vkStageFlags :: VkShaderStageFlags 
                              , vkPImmutableSamplers :: Ptr VkSampler 
                              }
  deriving (Eq)

instance Storable VkDescriptorSetLayoutBinding where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkDescriptorSetLayoutBinding <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 4) (vkDescriptorType (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 8) (vkDescriptorCount (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 12) (vkStageFlags (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 16) (vkPImmutableSamplers (poked :: VkDescriptorSetLayoutBinding))



data VkDescriptorSetLayoutCreateInfo =
  VkDescriptorSetLayoutCreateInfo{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkFlags :: VkDescriptorSetLayoutCreateFlags 
                                 , vkBindingCount :: Word32 
                                 , vkPBindings :: Ptr VkDescriptorSetLayoutBinding 
                                 }
  deriving (Eq)

instance Storable VkDescriptorSetLayoutCreateInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkDescriptorSetLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkBindingCount (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPBindings (poked :: VkDescriptorSetLayoutCreateInfo))



data VkDescriptorPoolSize =
  VkDescriptorPoolSize{ vkType :: VkDescriptorType 
                      , vkDescriptorCount :: Word32 
                      }
  deriving (Eq)

instance Storable VkDescriptorPoolSize where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkDescriptorPoolSize <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkDescriptorPoolSize))
                *> poke (ptr `plusPtr` 4) (vkDescriptorCount (poked :: VkDescriptorPoolSize))



data VkDescriptorPoolCreateInfo =
  VkDescriptorPoolCreateInfo{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkFlags :: VkDescriptorPoolCreateFlags 
                            , vkMaxSets :: Word32 
                            , vkPoolSizeCount :: Word32 
                            , vkPPoolSizes :: Ptr VkDescriptorPoolSize 
                            }
  deriving (Eq)

instance Storable VkDescriptorPoolCreateInfo where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkDescriptorPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMaxSets (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPoolSizeCount (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPPoolSizes (poked :: VkDescriptorPoolCreateInfo))



data VkDescriptorSetAllocateInfo =
  VkDescriptorSetAllocateInfo{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkDescriptorPool :: VkDescriptorPool 
                             , vkDescriptorSetCount :: Word32 
                             , vkPSetLayouts :: Ptr VkDescriptorSetLayout 
                             }
  deriving (Eq)

instance Storable VkDescriptorSetAllocateInfo where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkDescriptorSetAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkDescriptorPool (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkDescriptorSetCount (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSetLayouts (poked :: VkDescriptorSetAllocateInfo))



data VkSpecializationMapEntry =
  VkSpecializationMapEntry{ vkConstantID :: Word32 
                          , vkOffset :: Word32 
                          , vkSize :: CSize 
                          }
  deriving (Eq)

instance Storable VkSpecializationMapEntry where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = VkSpecializationMapEntry <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkConstantID (poked :: VkSpecializationMapEntry))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkSpecializationMapEntry))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSpecializationMapEntry))



data VkSpecializationInfo =
  VkSpecializationInfo{ vkMapEntryCount :: Word32 
                      , vkPMapEntries :: Ptr VkSpecializationMapEntry 
                      , vkDataSize :: CSize 
                      , vkPData :: Ptr Void 
                      }
  deriving (Eq)

instance Storable VkSpecializationInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkSpecializationInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMapEntryCount (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 8) (vkPMapEntries (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 16) (vkDataSize (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 24) (vkPData (poked :: VkSpecializationInfo))



data VkPipelineShaderStageCreateInfo =
  VkPipelineShaderStageCreateInfo{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkFlags :: VkPipelineShaderStageCreateFlags 
                                 , vkStage :: VkShaderStageFlagBits 
                                 , vkModule :: VkShaderModule 
                                 , vkPName :: Ptr CChar 
                                 , vkPSpecializationInfo :: Ptr VkSpecializationInfo 
                                 }
  deriving (Eq)

instance Storable VkPipelineShaderStageCreateInfo where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkPipelineShaderStageCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkStage (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkModule (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPName (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPSpecializationInfo (poked :: VkPipelineShaderStageCreateInfo))



data VkComputePipelineCreateInfo =
  VkComputePipelineCreateInfo{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkFlags :: VkPipelineCreateFlags 
                             , vkStage :: VkPipelineShaderStageCreateInfo 
                             , vkLayout :: VkPipelineLayout 
                             , vkBasePipelineHandle :: VkPipeline 
                             , vkBasePipelineIndex :: Int32 
                             }
  deriving (Eq)

instance Storable VkComputePipelineCreateInfo where
  sizeOf _ = 96
  alignment _ = 8
  peek ptr = VkComputePipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 72)
                                         <*> peek (ptr `plusPtr` 80)
                                         <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkStage (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkLayout (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkBasePipelineHandle (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (vkBasePipelineIndex (poked :: VkComputePipelineCreateInfo))



data VkVertexInputBindingDescription =
  VkVertexInputBindingDescription{ vkBinding :: Word32 
                                 , vkStride :: Word32 
                                 , vkInputRate :: VkVertexInputRate 
                                 }
  deriving (Eq)

instance Storable VkVertexInputBindingDescription where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkVertexInputBindingDescription <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 4)
                                             <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkVertexInputBindingDescription))
                *> poke (ptr `plusPtr` 4) (vkStride (poked :: VkVertexInputBindingDescription))
                *> poke (ptr `plusPtr` 8) (vkInputRate (poked :: VkVertexInputBindingDescription))



data VkVertexInputAttributeDescription =
  VkVertexInputAttributeDescription{ vkLocation :: Word32 
                                   , vkBinding :: Word32 
                                   , vkFormat :: VkFormat 
                                   , vkOffset :: Word32 
                                   }
  deriving (Eq)

instance Storable VkVertexInputAttributeDescription where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = VkVertexInputAttributeDescription <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLocation (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 4) (vkBinding (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 8) (vkFormat (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 12) (vkOffset (poked :: VkVertexInputAttributeDescription))



data VkPipelineVertexInputStateCreateInfo =
  VkPipelineVertexInputStateCreateInfo{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkFlags :: VkPipelineVertexInputStateCreateFlags 
                                      , vkVertexBindingDescriptionCount :: Word32 
                                      , vkPVertexBindingDescriptions :: Ptr VkVertexInputBindingDescription 
                                      , vkVertexAttributeDescriptionCount :: Word32 
                                      , vkPVertexAttributeDescriptions :: Ptr VkVertexInputAttributeDescription 
                                      }
  deriving (Eq)

instance Storable VkPipelineVertexInputStateCreateInfo where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkPipelineVertexInputStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkVertexBindingDescriptionCount (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPVertexBindingDescriptions (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkVertexAttributeDescriptionCount (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPVertexAttributeDescriptions (poked :: VkPipelineVertexInputStateCreateInfo))



data VkPipelineInputAssemblyStateCreateInfo =
  VkPipelineInputAssemblyStateCreateInfo{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkFlags :: VkPipelineInputAssemblyStateCreateFlags 
                                        , vkTopology :: VkPrimitiveTopology 
                                        , vkPrimitiveRestartEnable :: VkBool32 
                                        }
  deriving (Eq)

instance Storable VkPipelineInputAssemblyStateCreateInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkPipelineInputAssemblyStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkTopology (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPrimitiveRestartEnable (poked :: VkPipelineInputAssemblyStateCreateInfo))



data VkPipelineTessellationStateCreateInfo =
  VkPipelineTessellationStateCreateInfo{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkFlags :: VkPipelineTessellationStateCreateFlags 
                                       , vkPatchControlPoints :: Word32 
                                       }
  deriving (Eq)

instance Storable VkPipelineTessellationStateCreateInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkPipelineTessellationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkPatchControlPoints (poked :: VkPipelineTessellationStateCreateInfo))



data VkPipelineViewportStateCreateInfo =
  VkPipelineViewportStateCreateInfo{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkFlags :: VkPipelineViewportStateCreateFlags 
                                   , vkViewportCount :: Word32 
                                   , vkPViewports :: Ptr VkViewport 
                                   , vkScissorCount :: Word32 
                                   , vkPScissors :: Ptr VkRect2D 
                                   }
  deriving (Eq)

instance Storable VkPipelineViewportStateCreateInfo where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkPipelineViewportStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPViewports (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkScissorCount (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPScissors (poked :: VkPipelineViewportStateCreateInfo))



data VkPipelineRasterizationStateCreateInfo =
  VkPipelineRasterizationStateCreateInfo{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkFlags :: VkPipelineRasterizationStateCreateFlags 
                                        , vkDepthClampEnable :: VkBool32 
                                        , vkRasterizerDiscardEnable :: VkBool32 
                                        , vkPolygonMode :: VkPolygonMode 
                                        , vkCullMode :: VkCullModeFlags 
                                        , vkFrontFace :: VkFrontFace 
                                        , vkDepthBiasEnable :: VkBool32 
                                        , vkDepthBiasConstantFactor :: CFloat 
                                        , vkDepthBiasClamp :: CFloat 
                                        , vkDepthBiasSlopeFactor :: CFloat 
                                        , vkLineWidth :: CFloat 
                                        }
  deriving (Eq)

instance Storable VkPipelineRasterizationStateCreateInfo where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkPipelineRasterizationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 28)
                                                    <*> peek (ptr `plusPtr` 32)
                                                    <*> peek (ptr `plusPtr` 36)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 44)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 52)
                                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDepthClampEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkRasterizerDiscardEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkPolygonMode (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkCullMode (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkFrontFace (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkDepthBiasEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkDepthBiasConstantFactor (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkDepthBiasClamp (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkDepthBiasSlopeFactor (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkLineWidth (poked :: VkPipelineRasterizationStateCreateInfo))



data VkPipelineMultisampleStateCreateInfo =
  VkPipelineMultisampleStateCreateInfo{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkFlags :: VkPipelineMultisampleStateCreateFlags 
                                      , vkRasterizationSamples :: VkSampleCountFlagBits 
                                      , vkSampleShadingEnable :: VkBool32 
                                      , vkMinSampleShading :: CFloat 
                                      , vkPSampleMask :: Ptr VkSampleMask 
                                      , vkAlphaToCoverageEnable :: VkBool32 
                                      , vkAlphaToOneEnable :: VkBool32 
                                      }
  deriving (Eq)

instance Storable VkPipelineMultisampleStateCreateInfo where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkPipelineMultisampleStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 28)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
                                                  <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkRasterizationSamples (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSampleShadingEnable (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMinSampleShading (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSampleMask (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAlphaToCoverageEnable (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkAlphaToOneEnable (poked :: VkPipelineMultisampleStateCreateInfo))



data VkPipelineColorBlendAttachmentState =
  VkPipelineColorBlendAttachmentState{ vkBlendEnable :: VkBool32 
                                     , vkSrcColorBlendFactor :: VkBlendFactor 
                                     , vkDstColorBlendFactor :: VkBlendFactor 
                                     , vkColorBlendOp :: VkBlendOp 
                                     , vkSrcAlphaBlendFactor :: VkBlendFactor 
                                     , vkDstAlphaBlendFactor :: VkBlendFactor 
                                     , vkAlphaBlendOp :: VkBlendOp 
                                     , vkColorWriteMask :: VkColorComponentFlags 
                                     }
  deriving (Eq)

instance Storable VkPipelineColorBlendAttachmentState where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr = VkPipelineColorBlendAttachmentState <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 4)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 12)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBlendEnable (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 4) (vkSrcColorBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 8) (vkDstColorBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 12) (vkColorBlendOp (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 16) (vkSrcAlphaBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 20) (vkDstAlphaBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 24) (vkAlphaBlendOp (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 28) (vkColorWriteMask (poked :: VkPipelineColorBlendAttachmentState))



data VkPipelineColorBlendStateCreateInfo =
  VkPipelineColorBlendStateCreateInfo{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkFlags :: VkPipelineColorBlendStateCreateFlags 
                                     , vkLogicOpEnable :: VkBool32 
                                     , vkLogicOp :: VkLogicOp 
                                     , vkAttachmentCount :: Word32 
                                     , vkPAttachments :: Ptr VkPipelineColorBlendAttachmentState 
                                     , vkBlendConstants :: Vec (ToPeano 4) CFloat 
                                     }
  deriving (Eq)

instance Storable VkPipelineColorBlendStateCreateInfo where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkPipelineColorBlendStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
                                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkLogicOpEnable (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkLogicOp (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkAttachmentCount (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPAttachments (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkBlendConstants (poked :: VkPipelineColorBlendStateCreateInfo))



data VkPipelineDynamicStateCreateInfo =
  VkPipelineDynamicStateCreateInfo{ vkSType :: VkStructureType 
                                  , vkPNext :: Ptr Void 
                                  , vkFlags :: VkPipelineDynamicStateCreateFlags 
                                  , vkDynamicStateCount :: Word32 
                                  , vkPDynamicStates :: Ptr VkDynamicState 
                                  }
  deriving (Eq)

instance Storable VkPipelineDynamicStateCreateInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkPipelineDynamicStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDynamicStateCount (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPDynamicStates (poked :: VkPipelineDynamicStateCreateInfo))



data VkStencilOpState =
  VkStencilOpState{ vkFailOp :: VkStencilOp 
                  , vkPassOp :: VkStencilOp 
                  , vkDepthFailOp :: VkStencilOp 
                  , vkCompareOp :: VkCompareOp 
                  , vkCompareMask :: Word32 
                  , vkWriteMask :: Word32 
                  , vkReference :: Word32 
                  }
  deriving (Eq)

instance Storable VkStencilOpState where
  sizeOf _ = 28
  alignment _ = 4
  peek ptr = VkStencilOpState <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 12)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 20)
                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFailOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 4) (vkPassOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 8) (vkDepthFailOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 12) (vkCompareOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 16) (vkCompareMask (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 20) (vkWriteMask (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 24) (vkReference (poked :: VkStencilOpState))



data VkPipelineDepthStencilStateCreateInfo =
  VkPipelineDepthStencilStateCreateInfo{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkFlags :: VkPipelineDepthStencilStateCreateFlags 
                                       , vkDepthTestEnable :: VkBool32 
                                       , vkDepthWriteEnable :: VkBool32 
                                       , vkDepthCompareOp :: VkCompareOp 
                                       , vkDepthBoundsTestEnable :: VkBool32 
                                       , vkStencilTestEnable :: VkBool32 
                                       , vkFront :: VkStencilOpState 
                                       , vkBack :: VkStencilOpState 
                                       , vkMinDepthBounds :: CFloat 
                                       , vkMaxDepthBounds :: CFloat 
                                       }
  deriving (Eq)

instance Storable VkPipelineDepthStencilStateCreateInfo where
  sizeOf _ = 104
  alignment _ = 8
  peek ptr = VkPipelineDepthStencilStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 28)
                                                   <*> peek (ptr `plusPtr` 32)
                                                   <*> peek (ptr `plusPtr` 36)
                                                   <*> peek (ptr `plusPtr` 40)
                                                   <*> peek (ptr `plusPtr` 68)
                                                   <*> peek (ptr `plusPtr` 96)
                                                   <*> peek (ptr `plusPtr` 100)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDepthTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkDepthWriteEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkDepthCompareOp (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkDepthBoundsTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkStencilTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkFront (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkBack (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 96) (vkMinDepthBounds (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 100) (vkMaxDepthBounds (poked :: VkPipelineDepthStencilStateCreateInfo))



data VkGraphicsPipelineCreateInfo =
  VkGraphicsPipelineCreateInfo{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkFlags :: VkPipelineCreateFlags 
                              , vkStageCount :: Word32 
                              , vkPStages :: Ptr VkPipelineShaderStageCreateInfo 
                              , vkPVertexInputState :: Ptr VkPipelineVertexInputStateCreateInfo 
                              , vkPInputAssemblyState :: Ptr VkPipelineInputAssemblyStateCreateInfo 
                              , vkPTessellationState :: Ptr VkPipelineTessellationStateCreateInfo 
                              , vkPViewportState :: Ptr VkPipelineViewportStateCreateInfo 
                              , vkPRasterizationState :: Ptr VkPipelineRasterizationStateCreateInfo 
                              , vkPMultisampleState :: Ptr VkPipelineMultisampleStateCreateInfo 
                              , vkPDepthStencilState :: Ptr VkPipelineDepthStencilStateCreateInfo 
                              , vkPColorBlendState :: Ptr VkPipelineColorBlendStateCreateInfo 
                              , vkPDynamicState :: Ptr VkPipelineDynamicStateCreateInfo 
                              , vkLayout :: VkPipelineLayout 
                              , vkRenderPass :: VkRenderPass 
                              , vkSubpass :: Word32 
                              , vkBasePipelineHandle :: VkPipeline 
                              , vkBasePipelineIndex :: Int32 
                              }
  deriving (Eq)

instance Storable VkGraphicsPipelineCreateInfo where
  sizeOf _ = 144
  alignment _ = 8
  peek ptr = VkGraphicsPipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 20)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 48)
                                          <*> peek (ptr `plusPtr` 56)
                                          <*> peek (ptr `plusPtr` 64)
                                          <*> peek (ptr `plusPtr` 72)
                                          <*> peek (ptr `plusPtr` 80)
                                          <*> peek (ptr `plusPtr` 88)
                                          <*> peek (ptr `plusPtr` 96)
                                          <*> peek (ptr `plusPtr` 104)
                                          <*> peek (ptr `plusPtr` 112)
                                          <*> peek (ptr `plusPtr` 120)
                                          <*> peek (ptr `plusPtr` 128)
                                          <*> peek (ptr `plusPtr` 136)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkStageCount (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPStages (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPVertexInputState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPInputAssemblyState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPTessellationState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPViewportState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPRasterizationState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkPMultisampleState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkPDepthStencilState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (vkPColorBlendState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 96) (vkPDynamicState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 104) (vkLayout (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 112) (vkRenderPass (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 120) (vkSubpass (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 128) (vkBasePipelineHandle (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 136) (vkBasePipelineIndex (poked :: VkGraphicsPipelineCreateInfo))



data VkPipelineCacheCreateInfo =
  VkPipelineCacheCreateInfo{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkFlags :: VkPipelineCacheCreateFlags 
                           , vkInitialDataSize :: CSize 
                           , vkPInitialData :: Ptr Void 
                           }
  deriving (Eq)

instance Storable VkPipelineCacheCreateInfo where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkPipelineCacheCreateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkPipelineCacheCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPInitialData (poked :: VkPipelineCacheCreateInfo))



data VkPushConstantRange =
  VkPushConstantRange{ vkStageFlags :: VkShaderStageFlags 
                     , vkOffset :: Word32 
                     , vkSize :: Word32 
                     }
  deriving (Eq)

instance Storable VkPushConstantRange where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkPushConstantRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkStageFlags (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkPushConstantRange))



data VkPipelineLayoutCreateInfo =
  VkPipelineLayoutCreateInfo{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkFlags :: VkPipelineLayoutCreateFlags 
                            , vkSetLayoutCount :: Word32 
                            , vkPSetLayouts :: Ptr VkDescriptorSetLayout 
                            , vkPushConstantRangeCount :: Word32 
                            , vkPPushConstantRanges :: Ptr VkPushConstantRange 
                            }
  deriving (Eq)

instance Storable VkPipelineLayoutCreateInfo where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkPipelineLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkSetLayoutCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPSetLayouts (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPushConstantRangeCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPPushConstantRanges (poked :: VkPipelineLayoutCreateInfo))



data VkSamplerCreateInfo =
  VkSamplerCreateInfo{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkFlags :: VkSamplerCreateFlags 
                     , vkMagFilter :: VkFilter 
                     , vkMinFilter :: VkFilter 
                     , vkMipmapMode :: VkSamplerMipmapMode 
                     , vkAddressModeU :: VkSamplerAddressMode 
                     , vkAddressModeV :: VkSamplerAddressMode 
                     , vkAddressModeW :: VkSamplerAddressMode 
                     , vkMipLodBias :: CFloat 
                     , vkAnisotropyEnable :: VkBool32 
                     , vkMaxAnisotropy :: CFloat 
                     , vkCompareEnable :: VkBool32 
                     , vkCompareOp :: VkCompareOp 
                     , vkMinLod :: CFloat 
                     , vkMaxLod :: CFloat 
                     , vkBorderColor :: VkBorderColor 
                     , vkUnnormalizedCoordinates :: VkBool32 
                     }
  deriving (Eq)

instance Storable VkSamplerCreateInfo where
  sizeOf _ = 80
  alignment _ = 8
  peek ptr = VkSamplerCreateInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 36)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
                                 <*> peek (ptr `plusPtr` 52)
                                 <*> peek (ptr `plusPtr` 56)
                                 <*> peek (ptr `plusPtr` 60)
                                 <*> peek (ptr `plusPtr` 64)
                                 <*> peek (ptr `plusPtr` 68)
                                 <*> peek (ptr `plusPtr` 72)
                                 <*> peek (ptr `plusPtr` 76)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMagFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkMinFilter (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMipmapMode (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAddressModeU (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkAddressModeV (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAddressModeW (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkMipLodBias (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkAnisotropyEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkMaxAnisotropy (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkCompareEnable (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 60) (vkCompareOp (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkMinLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkMaxLod (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkBorderColor (poked :: VkSamplerCreateInfo))
                *> poke (ptr `plusPtr` 76) (vkUnnormalizedCoordinates (poked :: VkSamplerCreateInfo))



data VkCommandPoolCreateInfo =
  VkCommandPoolCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkCommandPoolCreateFlags 
                         , vkQueueFamilyIndex :: Word32 
                         }
  deriving (Eq)

instance Storable VkCommandPoolCreateInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkCommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkCommandPoolCreateInfo))



data VkCommandBufferAllocateInfo =
  VkCommandBufferAllocateInfo{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkCommandPool :: VkCommandPool 
                             , vkLevel :: VkCommandBufferLevel 
                             , vkCommandBufferCount :: Word32 
                             }
  deriving (Eq)

instance Storable VkCommandBufferAllocateInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkCommandBufferAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkCommandPool (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkLevel (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 28) (vkCommandBufferCount (poked :: VkCommandBufferAllocateInfo))



data VkCommandBufferInheritanceInfo =
  VkCommandBufferInheritanceInfo{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkRenderPass :: VkRenderPass 
                                , vkSubpass :: Word32 
                                , vkFramebuffer :: VkFramebuffer 
                                , vkOcclusionQueryEnable :: VkBool32 
                                , vkQueryFlags :: VkQueryControlFlags 
                                , vkPipelineStatistics :: VkQueryPipelineStatisticFlags 
                                }
  deriving (Eq)

instance Storable VkCommandBufferInheritanceInfo where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkCommandBufferInheritanceInfo <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
                                            <*> peek (ptr `plusPtr` 40)
                                            <*> peek (ptr `plusPtr` 44)
                                            <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 24) (vkSubpass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 32) (vkFramebuffer (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 40) (vkOcclusionQueryEnable (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 44) (vkQueryFlags (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 48) (vkPipelineStatistics (poked :: VkCommandBufferInheritanceInfo))



data VkCommandBufferBeginInfo =
  VkCommandBufferBeginInfo{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkCommandBufferUsageFlags 
                          , vkPInheritanceInfo :: Ptr VkCommandBufferInheritanceInfo 
                          }
  deriving (Eq)

instance Storable VkCommandBufferBeginInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkPInheritanceInfo (poked :: VkCommandBufferBeginInfo))



data VkRenderPassBeginInfo =
  VkRenderPassBeginInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkRenderPass :: VkRenderPass 
                       , vkFramebuffer :: VkFramebuffer 
                       , vkRenderArea :: VkRect2D 
                       , vkClearValueCount :: Word32 
                       , vkPClearValues :: Ptr VkClearValue 
                       }
  deriving (Eq)

instance Storable VkRenderPassBeginInfo where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkRenderPassBeginInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 48)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkFramebuffer (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (vkRenderArea (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (vkClearValueCount (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (vkPClearValues (poked :: VkRenderPassBeginInfo))



data VkClearDepthStencilValue =
  VkClearDepthStencilValue{ vkDepth :: CFloat 
                          , vkStencil :: Word32 
                          }
  deriving (Eq)

instance Storable VkClearDepthStencilValue where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDepth (poked :: VkClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (vkStencil (poked :: VkClearDepthStencilValue))



data VkClearAttachment =
  VkClearAttachment{ vkAspectMask :: VkImageAspectFlags 
                   , vkColorAttachment :: Word32 
                   , vkClearValue :: VkClearValue 
                   }
  deriving (Eq)

instance Storable VkClearAttachment where
  sizeOf _ = 24
  alignment _ = 4
  peek ptr = VkClearAttachment <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 4) (vkColorAttachment (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 8) (vkClearValue (poked :: VkClearAttachment))



data VkAttachmentDescription =
  VkAttachmentDescription{ vkFlags :: VkAttachmentDescriptionFlags 
                         , vkFormat :: VkFormat 
                         , vkSamples :: VkSampleCountFlagBits 
                         , vkLoadOp :: VkAttachmentLoadOp 
                         , vkStoreOp :: VkAttachmentStoreOp 
                         , vkStencilLoadOp :: VkAttachmentLoadOp 
                         , vkStencilStoreOp :: VkAttachmentStoreOp 
                         , vkInitialLayout :: VkImageLayout 
                         , vkFinalLayout :: VkImageLayout 
                         }
  deriving (Eq)

instance Storable VkAttachmentDescription where
  sizeOf _ = 36
  alignment _ = 4
  peek ptr = VkAttachmentDescription <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 28)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 4) (vkFormat (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 8) (vkSamples (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 12) (vkLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 16) (vkStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 20) (vkStencilLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 24) (vkStencilStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 28) (vkInitialLayout (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 32) (vkFinalLayout (poked :: VkAttachmentDescription))



data VkAttachmentReference =
  VkAttachmentReference{ vkAttachment :: Word32 
                       , vkLayout :: VkImageLayout 
                       }
  deriving (Eq)

instance Storable VkAttachmentReference where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkAttachmentReference <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachment (poked :: VkAttachmentReference))
                *> poke (ptr `plusPtr` 4) (vkLayout (poked :: VkAttachmentReference))



data VkSubpassDescription =
  VkSubpassDescription{ vkFlags :: VkSubpassDescriptionFlags 
                      , vkPipelineBindPoint :: VkPipelineBindPoint 
                      , vkInputAttachmentCount :: Word32 
                      , vkPInputAttachments :: Ptr VkAttachmentReference 
                      , vkColorAttachmentCount :: Word32 
                      , vkPColorAttachments :: Ptr VkAttachmentReference 
                      , vkPResolveAttachments :: Ptr VkAttachmentReference 
                      , vkPDepthStencilAttachment :: Ptr VkAttachmentReference 
                      , vkPreserveAttachmentCount :: Word32 
                      , vkPPreserveAttachments :: Ptr Word32 
                      }
  deriving (Eq)

instance Storable VkSubpassDescription where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr = VkSubpassDescription <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
                                  <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 4) (vkPipelineBindPoint (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 8) (vkInputAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 16) (vkPInputAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 24) (vkColorAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 32) (vkPColorAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 40) (vkPResolveAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 48) (vkPDepthStencilAttachment (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 56) (vkPreserveAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 64) (vkPPreserveAttachments (poked :: VkSubpassDescription))



data VkSubpassDependency =
  VkSubpassDependency{ vkSrcSubpass :: Word32 
                     , vkDstSubpass :: Word32 
                     , vkSrcStageMask :: VkPipelineStageFlags 
                     , vkDstStageMask :: VkPipelineStageFlags 
                     , vkSrcAccessMask :: VkAccessFlags 
                     , vkDstAccessMask :: VkAccessFlags 
                     , vkDependencyFlags :: VkDependencyFlags 
                     }
  deriving (Eq)

instance Storable VkSubpassDependency where
  sizeOf _ = 28
  alignment _ = 4
  peek ptr = VkSubpassDependency <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 12)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 4) (vkDstSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 8) (vkSrcStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 12) (vkDstStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 24) (vkDependencyFlags (poked :: VkSubpassDependency))



data VkRenderPassCreateInfo =
  VkRenderPassCreateInfo{ vkSType :: VkStructureType 
                        , vkPNext :: Ptr Void 
                        , vkFlags :: VkRenderPassCreateFlags 
                        , vkAttachmentCount :: Word32 
                        , vkPAttachments :: Ptr VkAttachmentDescription 
                        , vkSubpassCount :: Word32 
                        , vkPSubpasses :: Ptr VkSubpassDescription 
                        , vkDependencyCount :: Word32 
                        , vkPDependencies :: Ptr VkSubpassDependency 
                        }
  deriving (Eq)

instance Storable VkRenderPassCreateInfo where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkRenderPassCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 20)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkAttachmentCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPAttachments (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkSubpassCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPSubpasses (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkDependencyCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPDependencies (poked :: VkRenderPassCreateInfo))



data VkEventCreateInfo =
  VkEventCreateInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkFlags :: VkEventCreateFlags 
                   }
  deriving (Eq)

instance Storable VkEventCreateInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkEventCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkEventCreateInfo))



data VkFenceCreateInfo =
  VkFenceCreateInfo{ vkSType :: VkStructureType 
                   , vkPNext :: Ptr Void 
                   , vkFlags :: VkFenceCreateFlags 
                   }
  deriving (Eq)

instance Storable VkFenceCreateInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFenceCreateInfo))



data VkPhysicalDeviceFeatures =
  VkPhysicalDeviceFeatures{ vkRobustBufferAccess :: VkBool32 
                          , vkFullDrawIndexUint :: VkBool32 
                          , vkImageCubeArray :: VkBool32 
                          , vkIndependentBlend :: VkBool32 
                          , vkGeometryShader :: VkBool32 
                          , vkTessellationShader :: VkBool32 
                          , vkSampleRateShading :: VkBool32 
                          , vkDualSrcBlend :: VkBool32 
                          , vkLogicOp :: VkBool32 
                          , vkMultiDrawIndirect :: VkBool32 
                          , vkDrawIndirectFirstInstance :: VkBool32 
                          , vkDepthClamp :: VkBool32 
                          , vkDepthBiasClamp :: VkBool32 
                          , vkFillModeNonSolid :: VkBool32 
                          , vkDepthBounds :: VkBool32 
                          , vkWideLines :: VkBool32 
                          , vkLargePoints :: VkBool32 
                          , vkAlphaToOne :: VkBool32 
                          , vkMultiViewport :: VkBool32 
                          , vkSamplerAnisotropy :: VkBool32 
                          , vkTextureCompressionETC :: VkBool32 
                          , vkTextureCompressionASTC_LDR :: VkBool32 
                          , vkTextureCompressionBC :: VkBool32 
                          , vkOcclusionQueryPrecise :: VkBool32 
                          , vkPipelineStatisticsQuery :: VkBool32 
                          , vkVertexPipelineStoresAndAtomics :: VkBool32 
                          , vkFragmentStoresAndAtomics :: VkBool32 
                          , vkShaderTessellationAndGeometryPointSize :: VkBool32 
                          , vkShaderImageGatherExtended :: VkBool32 
                          , vkShaderStorageImageExtendedFormats :: VkBool32 
                          , vkShaderStorageImageMultisample :: VkBool32 
                          , vkShaderStorageImageReadWithoutFormat :: VkBool32 
                          , vkShaderStorageImageWriteWithoutFormat :: VkBool32 
                          , vkShaderUniformBufferArrayDynamicIndexing :: VkBool32 
                          , vkShaderSampledImageArrayDynamicIndexing :: VkBool32 
                          , vkShaderStorageBufferArrayDynamicIndexing :: VkBool32 
                          , vkShaderStorageImageArrayDynamicIndexing :: VkBool32 
                          , vkShaderClipDistance :: VkBool32 
                          , vkShaderCullDistance :: VkBool32 
                          , vkShaderFloat :: VkBool32 
                          , vkShaderInt :: VkBool32 
                          , vkShaderInt :: VkBool32 
                          , vkShaderResourceResidency :: VkBool32 
                          , vkShaderResourceMinLod :: VkBool32 
                          , vkSparseBinding :: VkBool32 
                          , vkSparseResidencyBuffer :: VkBool32 
                          , vkSparseResidencyImage :: VkBool32 
                          , vkSparseResidencyImage :: VkBool32 
                          , vkSparseResidency :: VkBool32 
                          , vkSparseResidency :: VkBool32 
                          , vkSparseResidency :: VkBool32 
                          , vkSparseResidency :: VkBool32 
                          , vkSparseResidencyAliased :: VkBool32 
                          , vkVariableMultisampleRate :: VkBool32 
                          , vkInheritedQueries :: VkBool32 
                          }
  deriving (Eq)

instance Storable VkPhysicalDeviceFeatures where
  sizeOf _ = 220
  alignment _ = 4
  peek ptr = VkPhysicalDeviceFeatures <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 60)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 68)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 76)
                                      <*> peek (ptr `plusPtr` 80)
                                      <*> peek (ptr `plusPtr` 84)
                                      <*> peek (ptr `plusPtr` 88)
                                      <*> peek (ptr `plusPtr` 92)
                                      <*> peek (ptr `plusPtr` 96)
                                      <*> peek (ptr `plusPtr` 100)
                                      <*> peek (ptr `plusPtr` 104)
                                      <*> peek (ptr `plusPtr` 108)
                                      <*> peek (ptr `plusPtr` 112)
                                      <*> peek (ptr `plusPtr` 116)
                                      <*> peek (ptr `plusPtr` 120)
                                      <*> peek (ptr `plusPtr` 124)
                                      <*> peek (ptr `plusPtr` 128)
                                      <*> peek (ptr `plusPtr` 132)
                                      <*> peek (ptr `plusPtr` 136)
                                      <*> peek (ptr `plusPtr` 140)
                                      <*> peek (ptr `plusPtr` 144)
                                      <*> peek (ptr `plusPtr` 148)
                                      <*> peek (ptr `plusPtr` 152)
                                      <*> peek (ptr `plusPtr` 156)
                                      <*> peek (ptr `plusPtr` 160)
                                      <*> peek (ptr `plusPtr` 164)
                                      <*> peek (ptr `plusPtr` 168)
                                      <*> peek (ptr `plusPtr` 172)
                                      <*> peek (ptr `plusPtr` 176)
                                      <*> peek (ptr `plusPtr` 180)
                                      <*> peek (ptr `plusPtr` 184)
                                      <*> peek (ptr `plusPtr` 188)
                                      <*> peek (ptr `plusPtr` 192)
                                      <*> peek (ptr `plusPtr` 196)
                                      <*> peek (ptr `plusPtr` 200)
                                      <*> peek (ptr `plusPtr` 204)
                                      <*> peek (ptr `plusPtr` 208)
                                      <*> peek (ptr `plusPtr` 212)
                                      <*> peek (ptr `plusPtr` 216)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRobustBufferAccess (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 4) (vkFullDrawIndexUint (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 8) (vkImageCubeArray (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 12) (vkIndependentBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 16) (vkGeometryShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 20) (vkTessellationShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 24) (vkSampleRateShading (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 28) (vkDualSrcBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 32) (vkLogicOp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 36) (vkMultiDrawIndirect (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 40) (vkDrawIndirectFirstInstance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 44) (vkDepthClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 48) (vkDepthBiasClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 52) (vkFillModeNonSolid (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 56) (vkDepthBounds (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 60) (vkWideLines (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 64) (vkLargePoints (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 68) (vkAlphaToOne (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 72) (vkMultiViewport (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 76) (vkSamplerAnisotropy (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 80) (vkTextureCompressionETC (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 84) (vkTextureCompressionASTC_LDR (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 88) (vkTextureCompressionBC (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 92) (vkOcclusionQueryPrecise (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 96) (vkPipelineStatisticsQuery (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 100) (vkVertexPipelineStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 104) (vkFragmentStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 108) (vkShaderTessellationAndGeometryPointSize (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 112) (vkShaderImageGatherExtended (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 116) (vkShaderStorageImageExtendedFormats (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 120) (vkShaderStorageImageMultisample (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 124) (vkShaderStorageImageReadWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 128) (vkShaderStorageImageWriteWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 132) (vkShaderUniformBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 136) (vkShaderSampledImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 140) (vkShaderStorageBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 144) (vkShaderStorageImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 148) (vkShaderClipDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 152) (vkShaderCullDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 156) (vkShaderFloat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 160) (vkShaderInt (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 164) (vkShaderInt (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 168) (vkShaderResourceResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 172) (vkShaderResourceMinLod (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 176) (vkSparseBinding (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 180) (vkSparseResidencyBuffer (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 184) (vkSparseResidencyImage (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 188) (vkSparseResidencyImage (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 192) (vkSparseResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 196) (vkSparseResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 200) (vkSparseResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 204) (vkSparseResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 208) (vkSparseResidencyAliased (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 212) (vkVariableMultisampleRate (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 216) (vkInheritedQueries (poked :: VkPhysicalDeviceFeatures))



data VkPhysicalDeviceSparseProperties =
  VkPhysicalDeviceSparseProperties{ vkResidencyStandard :: VkBool32 
                                  , vkResidencyStandard :: VkBool32 
                                  , vkResidencyStandard :: VkBool32 
                                  , vkResidencyAlignedMipSize :: VkBool32 
                                  , vkResidencyNonResidentStrict :: VkBool32 
                                  }
  deriving (Eq)

instance Storable VkPhysicalDeviceSparseProperties where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = VkPhysicalDeviceSparseProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkResidencyStandard (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 4) (vkResidencyStandard (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 8) (vkResidencyStandard (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 12) (vkResidencyAlignedMipSize (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 16) (vkResidencyNonResidentStrict (poked :: VkPhysicalDeviceSparseProperties))



data VkPhysicalDeviceLimits =
  VkPhysicalDeviceLimits{ vkMaxImageDimension :: Word32 
                        , vkMaxImageDimension :: Word32 
                        , vkMaxImageDimension :: Word32 
                        , vkMaxImageDimensionCube :: Word32 
                        , vkMaxImageArrayLayers :: Word32 
                        , vkMaxTexelBufferElements :: Word32 
                        , vkMaxUniformBufferRange :: Word32 
                        , vkMaxStorageBufferRange :: Word32 
                        , vkMaxPushConstantsSize :: Word32 
                        , vkMaxMemoryAllocationCount :: Word32 
                        , vkMaxSamplerAllocationCount :: Word32 
                        , vkBufferImageGranularity :: VkDeviceSize 
                        , vkSparseAddressSpaceSize :: VkDeviceSize 
                        , vkMaxBoundDescriptorSets :: Word32 
                        , vkMaxPerStageDescriptorSamplers :: Word32 
                        , vkMaxPerStageDescriptorUniformBuffers :: Word32 
                        , vkMaxPerStageDescriptorStorageBuffers :: Word32 
                        , vkMaxPerStageDescriptorSampledImages :: Word32 
                        , vkMaxPerStageDescriptorStorageImages :: Word32 
                        , vkMaxPerStageDescriptorInputAttachments :: Word32 
                        , vkMaxPerStageResources :: Word32 
                        , vkMaxDescriptorSetSamplers :: Word32 
                        , vkMaxDescriptorSetUniformBuffers :: Word32 
                        , vkMaxDescriptorSetUniformBuffersDynamic :: Word32 
                        , vkMaxDescriptorSetStorageBuffers :: Word32 
                        , vkMaxDescriptorSetStorageBuffersDynamic :: Word32 
                        , vkMaxDescriptorSetSampledImages :: Word32 
                        , vkMaxDescriptorSetStorageImages :: Word32 
                        , vkMaxDescriptorSetInputAttachments :: Word32 
                        , vkMaxVertexInputAttributes :: Word32 
                        , vkMaxVertexInputBindings :: Word32 
                        , vkMaxVertexInputAttributeOffset :: Word32 
                        , vkMaxVertexInputBindingStride :: Word32 
                        , vkMaxVertexOutputComponents :: Word32 
                        , vkMaxTessellationGenerationLevel :: Word32 
                        , vkMaxTessellationPatchSize :: Word32 
                        , vkMaxTessellationControlPerVertexInputComponents :: Word32 
                        , vkMaxTessellationControlPerVertexOutputComponents :: Word32 
                        , vkMaxTessellationControlPerPatchOutputComponents :: Word32 
                        , vkMaxTessellationControlTotalOutputComponents :: Word32 
                        , vkMaxTessellationEvaluationInputComponents :: Word32 
                        , vkMaxTessellationEvaluationOutputComponents :: Word32 
                        , vkMaxGeometryShaderInvocations :: Word32 
                        , vkMaxGeometryInputComponents :: Word32 
                        , vkMaxGeometryOutputComponents :: Word32 
                        , vkMaxGeometryOutputVertices :: Word32 
                        , vkMaxGeometryTotalOutputComponents :: Word32 
                        , vkMaxFragmentInputComponents :: Word32 
                        , vkMaxFragmentOutputAttachments :: Word32 
                        , vkMaxFragmentDualSrcAttachments :: Word32 
                        , vkMaxFragmentCombinedOutputResources :: Word32 
                        , vkMaxComputeSharedMemorySize :: Word32 
                        , vkMaxComputeWorkGroupCount :: Vec (ToPeano 3) Word32 
                        , vkMaxComputeWorkGroupInvocations :: Word32 
                        , vkMaxComputeWorkGroupSize :: Vec (ToPeano 3) Word32 
                        , vkSubPixelPrecisionBits :: Word32 
                        , vkSubTexelPrecisionBits :: Word32 
                        , vkMipmapPrecisionBits :: Word32 
                        , vkMaxDrawIndexedIndexValue :: Word32 
                        , vkMaxDrawIndirectCount :: Word32 
                        , vkMaxSamplerLodBias :: CFloat 
                        , vkMaxSamplerAnisotropy :: CFloat 
                        , vkMaxViewports :: Word32 
                        , vkMaxViewportDimensions :: Vec (ToPeano 2) Word32 
                        , vkViewportBoundsRange :: Vec (ToPeano 2) CFloat 
                        , vkViewportSubPixelBits :: Word32 
                        , vkMinMemoryMapAlignment :: CSize 
                        , vkMinTexelBufferOffsetAlignment :: VkDeviceSize 
                        , vkMinUniformBufferOffsetAlignment :: VkDeviceSize 
                        , vkMinStorageBufferOffsetAlignment :: VkDeviceSize 
                        , vkMinTexelOffset :: Int32 
                        , vkMaxTexelOffset :: Word32 
                        , vkMinTexelGatherOffset :: Int32 
                        , vkMaxTexelGatherOffset :: Word32 
                        , vkMinInterpolationOffset :: CFloat 
                        , vkMaxInterpolationOffset :: CFloat 
                        , vkSubPixelInterpolationOffsetBits :: Word32 
                        , vkMaxFramebufferWidth :: Word32 
                        , vkMaxFramebufferHeight :: Word32 
                        , vkMaxFramebufferLayers :: Word32 
                        , vkFramebufferColorSampleCounts :: VkSampleCountFlags 
                        , vkFramebufferDepthSampleCounts :: VkSampleCountFlags 
                        , vkFramebufferStencilSampleCounts :: VkSampleCountFlags 
                        , vkFramebufferNoAttachmentsSampleCounts :: VkSampleCountFlags 
                        , vkMaxColorAttachments :: Word32 
                        , vkSampledImageColorSampleCounts :: VkSampleCountFlags 
                        , vkSampledImageIntegerSampleCounts :: VkSampleCountFlags 
                        , vkSampledImageDepthSampleCounts :: VkSampleCountFlags 
                        , vkSampledImageStencilSampleCounts :: VkSampleCountFlags 
                        , vkStorageImageSampleCounts :: VkSampleCountFlags 
                        , vkMaxSampleMaskWords :: Word32 
                        , vkTimestampComputeAndGraphics :: VkBool32 
                        , vkTimestampPeriod :: CFloat 
                        , vkMaxClipDistances :: Word32 
                        , vkMaxCullDistances :: Word32 
                        , vkMaxCombinedClipAndCullDistances :: Word32 
                        , vkDiscreteQueuePriorities :: Word32 
                        , vkPointSizeRange :: Vec (ToPeano 2) CFloat 
                        , vkLineWidthRange :: Vec (ToPeano 2) CFloat 
                        , vkPointSizeGranularity :: CFloat 
                        , vkLineWidthGranularity :: CFloat 
                        , vkStrictLines :: VkBool32 
                        , vkStandardSampleLocations :: VkBool32 
                        , vkOptimalBufferCopyOffsetAlignment :: VkDeviceSize 
                        , vkOptimalBufferCopyRowPitchAlignment :: VkDeviceSize 
                        , vkNonCoherentAtomSize :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkPhysicalDeviceLimits where
  sizeOf _ = 504
  alignment _ = 8
  peek ptr = VkPhysicalDeviceLimits <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 12)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 20)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 28)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 56)
                                    <*> peek (ptr `plusPtr` 64)
                                    <*> peek (ptr `plusPtr` 68)
                                    <*> peek (ptr `plusPtr` 72)
                                    <*> peek (ptr `plusPtr` 76)
                                    <*> peek (ptr `plusPtr` 80)
                                    <*> peek (ptr `plusPtr` 84)
                                    <*> peek (ptr `plusPtr` 88)
                                    <*> peek (ptr `plusPtr` 92)
                                    <*> peek (ptr `plusPtr` 96)
                                    <*> peek (ptr `plusPtr` 100)
                                    <*> peek (ptr `plusPtr` 104)
                                    <*> peek (ptr `plusPtr` 108)
                                    <*> peek (ptr `plusPtr` 112)
                                    <*> peek (ptr `plusPtr` 116)
                                    <*> peek (ptr `plusPtr` 120)
                                    <*> peek (ptr `plusPtr` 124)
                                    <*> peek (ptr `plusPtr` 128)
                                    <*> peek (ptr `plusPtr` 132)
                                    <*> peek (ptr `plusPtr` 136)
                                    <*> peek (ptr `plusPtr` 140)
                                    <*> peek (ptr `plusPtr` 144)
                                    <*> peek (ptr `plusPtr` 148)
                                    <*> peek (ptr `plusPtr` 152)
                                    <*> peek (ptr `plusPtr` 156)
                                    <*> peek (ptr `plusPtr` 160)
                                    <*> peek (ptr `plusPtr` 164)
                                    <*> peek (ptr `plusPtr` 168)
                                    <*> peek (ptr `plusPtr` 172)
                                    <*> peek (ptr `plusPtr` 176)
                                    <*> peek (ptr `plusPtr` 180)
                                    <*> peek (ptr `plusPtr` 184)
                                    <*> peek (ptr `plusPtr` 188)
                                    <*> peek (ptr `plusPtr` 192)
                                    <*> peek (ptr `plusPtr` 196)
                                    <*> peek (ptr `plusPtr` 200)
                                    <*> peek (ptr `plusPtr` 204)
                                    <*> peek (ptr `plusPtr` 208)
                                    <*> peek (ptr `plusPtr` 212)
                                    <*> peek (ptr `plusPtr` 216)
                                    <*> peek (ptr `plusPtr` 220)
                                    <*> peek (ptr `plusPtr` 232)
                                    <*> peek (ptr `plusPtr` 236)
                                    <*> peek (ptr `plusPtr` 248)
                                    <*> peek (ptr `plusPtr` 252)
                                    <*> peek (ptr `plusPtr` 256)
                                    <*> peek (ptr `plusPtr` 260)
                                    <*> peek (ptr `plusPtr` 264)
                                    <*> peek (ptr `plusPtr` 268)
                                    <*> peek (ptr `plusPtr` 272)
                                    <*> peek (ptr `plusPtr` 276)
                                    <*> peek (ptr `plusPtr` 280)
                                    <*> peek (ptr `plusPtr` 288)
                                    <*> peek (ptr `plusPtr` 296)
                                    <*> peek (ptr `plusPtr` 304)
                                    <*> peek (ptr `plusPtr` 312)
                                    <*> peek (ptr `plusPtr` 320)
                                    <*> peek (ptr `plusPtr` 328)
                                    <*> peek (ptr `plusPtr` 336)
                                    <*> peek (ptr `plusPtr` 340)
                                    <*> peek (ptr `plusPtr` 344)
                                    <*> peek (ptr `plusPtr` 348)
                                    <*> peek (ptr `plusPtr` 352)
                                    <*> peek (ptr `plusPtr` 356)
                                    <*> peek (ptr `plusPtr` 360)
                                    <*> peek (ptr `plusPtr` 364)
                                    <*> peek (ptr `plusPtr` 368)
                                    <*> peek (ptr `plusPtr` 372)
                                    <*> peek (ptr `plusPtr` 376)
                                    <*> peek (ptr `plusPtr` 380)
                                    <*> peek (ptr `plusPtr` 384)
                                    <*> peek (ptr `plusPtr` 388)
                                    <*> peek (ptr `plusPtr` 392)
                                    <*> peek (ptr `plusPtr` 396)
                                    <*> peek (ptr `plusPtr` 400)
                                    <*> peek (ptr `plusPtr` 404)
                                    <*> peek (ptr `plusPtr` 408)
                                    <*> peek (ptr `plusPtr` 412)
                                    <*> peek (ptr `plusPtr` 416)
                                    <*> peek (ptr `plusPtr` 420)
                                    <*> peek (ptr `plusPtr` 424)
                                    <*> peek (ptr `plusPtr` 428)
                                    <*> peek (ptr `plusPtr` 432)
                                    <*> peek (ptr `plusPtr` 436)
                                    <*> peek (ptr `plusPtr` 440)
                                    <*> peek (ptr `plusPtr` 444)
                                    <*> peek (ptr `plusPtr` 452)
                                    <*> peek (ptr `plusPtr` 460)
                                    <*> peek (ptr `plusPtr` 464)
                                    <*> peek (ptr `plusPtr` 468)
                                    <*> peek (ptr `plusPtr` 472)
                                    <*> peek (ptr `plusPtr` 480)
                                    <*> peek (ptr `plusPtr` 488)
                                    <*> peek (ptr `plusPtr` 496)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxImageDimension (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 4) (vkMaxImageDimension (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 8) (vkMaxImageDimension (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 12) (vkMaxImageDimensionCube (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 16) (vkMaxImageArrayLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 20) (vkMaxTexelBufferElements (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 24) (vkMaxUniformBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 28) (vkMaxStorageBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 32) (vkMaxPushConstantsSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 36) (vkMaxMemoryAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 40) (vkMaxSamplerAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 48) (vkBufferImageGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 56) (vkSparseAddressSpaceSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 64) (vkMaxBoundDescriptorSets (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 68) (vkMaxPerStageDescriptorSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 72) (vkMaxPerStageDescriptorUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 76) (vkMaxPerStageDescriptorStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 80) (vkMaxPerStageDescriptorSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 84) (vkMaxPerStageDescriptorStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 88) (vkMaxPerStageDescriptorInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 92) (vkMaxPerStageResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 96) (vkMaxDescriptorSetSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 100) (vkMaxDescriptorSetUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 104) (vkMaxDescriptorSetUniformBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 108) (vkMaxDescriptorSetStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 112) (vkMaxDescriptorSetStorageBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 116) (vkMaxDescriptorSetSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 120) (vkMaxDescriptorSetStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 124) (vkMaxDescriptorSetInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 128) (vkMaxVertexInputAttributes (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 132) (vkMaxVertexInputBindings (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 136) (vkMaxVertexInputAttributeOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 140) (vkMaxVertexInputBindingStride (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 144) (vkMaxVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 148) (vkMaxTessellationGenerationLevel (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 152) (vkMaxTessellationPatchSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 156) (vkMaxTessellationControlPerVertexInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 160) (vkMaxTessellationControlPerVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 164) (vkMaxTessellationControlPerPatchOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 168) (vkMaxTessellationControlTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 172) (vkMaxTessellationEvaluationInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 176) (vkMaxTessellationEvaluationOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 180) (vkMaxGeometryShaderInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 184) (vkMaxGeometryInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 188) (vkMaxGeometryOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 192) (vkMaxGeometryOutputVertices (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 196) (vkMaxGeometryTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 200) (vkMaxFragmentInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 204) (vkMaxFragmentOutputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 208) (vkMaxFragmentDualSrcAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 212) (vkMaxFragmentCombinedOutputResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 216) (vkMaxComputeSharedMemorySize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 220) (vkMaxComputeWorkGroupCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 232) (vkMaxComputeWorkGroupInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 236) (vkMaxComputeWorkGroupSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 248) (vkSubPixelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 252) (vkSubTexelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 256) (vkMipmapPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 260) (vkMaxDrawIndexedIndexValue (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 264) (vkMaxDrawIndirectCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 268) (vkMaxSamplerLodBias (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 272) (vkMaxSamplerAnisotropy (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 276) (vkMaxViewports (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 280) (vkMaxViewportDimensions (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 288) (vkViewportBoundsRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 296) (vkViewportSubPixelBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 304) (vkMinMemoryMapAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 312) (vkMinTexelBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 320) (vkMinUniformBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 328) (vkMinStorageBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 336) (vkMinTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 340) (vkMaxTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 344) (vkMinTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 348) (vkMaxTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 352) (vkMinInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 356) (vkMaxInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 360) (vkSubPixelInterpolationOffsetBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 364) (vkMaxFramebufferWidth (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 368) (vkMaxFramebufferHeight (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 372) (vkMaxFramebufferLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 376) (vkFramebufferColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 380) (vkFramebufferDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 384) (vkFramebufferStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 388) (vkFramebufferNoAttachmentsSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 392) (vkMaxColorAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 396) (vkSampledImageColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 400) (vkSampledImageIntegerSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 404) (vkSampledImageDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 408) (vkSampledImageStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 412) (vkStorageImageSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 416) (vkMaxSampleMaskWords (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 420) (vkTimestampComputeAndGraphics (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 424) (vkTimestampPeriod (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 428) (vkMaxClipDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 432) (vkMaxCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 436) (vkMaxCombinedClipAndCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 440) (vkDiscreteQueuePriorities (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 444) (vkPointSizeRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 452) (vkLineWidthRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 460) (vkPointSizeGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 464) (vkLineWidthGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 468) (vkStrictLines (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 472) (vkStandardSampleLocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 480) (vkOptimalBufferCopyOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 488) (vkOptimalBufferCopyRowPitchAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 496) (vkNonCoherentAtomSize (poked :: VkPhysicalDeviceLimits))



data VkSemaphoreCreateInfo =
  VkSemaphoreCreateInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkFlags :: VkSemaphoreCreateFlags 
                       }
  deriving (Eq)

instance Storable VkSemaphoreCreateInfo where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSemaphoreCreateInfo))



data VkQueryPoolCreateInfo =
  VkQueryPoolCreateInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkFlags :: VkQueryPoolCreateFlags 
                       , vkQueryType :: VkQueryType 
                       , vkQueryCount :: Word32 
                       , vkPipelineStatistics :: VkQueryPipelineStatisticFlags 
                       }
  deriving (Eq)

instance Storable VkQueryPoolCreateInfo where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkQueryPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueryType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueryCount (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkPipelineStatistics (poked :: VkQueryPoolCreateInfo))



data VkFramebufferCreateInfo =
  VkFramebufferCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkFramebufferCreateFlags 
                         , vkRenderPass :: VkRenderPass 
                         , vkAttachmentCount :: Word32 
                         , vkPAttachments :: Ptr VkImageView 
                         , vkWidth :: Word32 
                         , vkHeight :: Word32 
                         , vkLayers :: Word32 
                         }
  deriving (Eq)

instance Storable VkFramebufferCreateInfo where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkFramebufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 52)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkRenderPass (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAttachmentCount (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPAttachments (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkWidth (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkHeight (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkLayers (poked :: VkFramebufferCreateInfo))



data VkDrawIndirectCommand =
  VkDrawIndirectCommand{ vkVertexCount :: Word32 
                       , vkInstanceCount :: Word32 
                       , vkFirstVertex :: Word32 
                       , vkFirstInstance :: Word32 
                       }
  deriving (Eq)

instance Storable VkDrawIndirectCommand where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = VkDrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVertexCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstVertex (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkFirstInstance (poked :: VkDrawIndirectCommand))



data VkDrawIndexedIndirectCommand =
  VkDrawIndexedIndirectCommand{ vkIndexCount :: Word32 
                              , vkInstanceCount :: Word32 
                              , vkFirstIndex :: Word32 
                              , vkVertexOffset :: Int32 
                              , vkFirstInstance :: Word32 
                              }
  deriving (Eq)

instance Storable VkDrawIndexedIndirectCommand where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = VkDrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkIndexCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstIndex (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkVertexOffset (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (vkFirstInstance (poked :: VkDrawIndexedIndirectCommand))



data VkDispatchIndirectCommand =
  VkDispatchIndirectCommand{ vkX :: Word32 
                           , vkY :: Word32 
                           , vkZ :: Word32 
                           }
  deriving (Eq)

instance Storable VkDispatchIndirectCommand where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkDispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 4)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkDispatchIndirectCommand))



data VkSubmitInfo =
  VkSubmitInfo{ vkSType :: VkStructureType 
              , vkPNext :: Ptr Void 
              , vkWaitSemaphoreCount :: Word32 
              , vkPWaitSemaphores :: Ptr VkSemaphore 
              , vkPWaitDstStageMask :: Ptr VkPipelineStageFlags 
              , vkCommandBufferCount :: Word32 
              , vkPCommandBuffers :: Ptr VkCommandBuffer 
              , vkSignalSemaphoreCount :: Word32 
              , vkPSignalSemaphores :: Ptr VkSemaphore 
              }
  deriving (Eq)

instance Storable VkSubmitInfo where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr = VkSubmitInfo <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
                          <*> peek (ptr `plusPtr` 24)
                          <*> peek (ptr `plusPtr` 32)
                          <*> peek (ptr `plusPtr` 40)
                          <*> peek (ptr `plusPtr` 48)
                          <*> peek (ptr `plusPtr` 56)
                          <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkPWaitDstStageMask (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkCommandBufferCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkPCommandBuffers (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkSignalSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 64) (vkPSignalSemaphores (poked :: VkSubmitInfo))



data VkDisplayPropertiesKHR =
  VkDisplayPropertiesKHR{ vkDisplay :: VkDisplayKHR 
                        , vkDisplayName :: Ptr CChar 
                        , vkPhysicalDimensions :: VkExtent2D 
                        , vkPhysicalResolution :: VkExtent2D 
                        , vkSupportedTransforms :: VkSurfaceTransformFlagsKHR 
                        , vkPlaneReorderPossible :: VkBool32 
                        , vkPersistentContent :: VkBool32 
                        }
  deriving (Eq)

instance Storable VkDisplayPropertiesKHR where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr = VkDisplayPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplay (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkDisplayName (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDimensions (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkPhysicalResolution (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (vkSupportedTransforms (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneReorderPossible (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (vkPersistentContent (poked :: VkDisplayPropertiesKHR))



data VkDisplayPlanePropertiesKHR =
  VkDisplayPlanePropertiesKHR{ vkCurrentDisplay :: VkDisplayKHR 
                             , vkCurrentStackIndex :: Word32 
                             }
  deriving (Eq)

instance Storable VkDisplayPlanePropertiesKHR where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = VkDisplayPlanePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkCurrentDisplay (poked :: VkDisplayPlanePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentStackIndex (poked :: VkDisplayPlanePropertiesKHR))



data VkDisplayModeParametersKHR =
  VkDisplayModeParametersKHR{ vkVisibleRegion :: VkExtent2D 
                            , vkRefreshRate :: Word32 
                            }
  deriving (Eq)

instance Storable VkDisplayModeParametersKHR where
  sizeOf _ = 12
  alignment _ = 4
  peek ptr = VkDisplayModeParametersKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVisibleRegion (poked :: VkDisplayModeParametersKHR))
                *> poke (ptr `plusPtr` 8) (vkRefreshRate (poked :: VkDisplayModeParametersKHR))



data VkDisplayModePropertiesKHR =
  VkDisplayModePropertiesKHR{ vkDisplayMode :: VkDisplayModeKHR 
                            , vkParameters :: VkDisplayModeParametersKHR 
                            }
  deriving (Eq)

instance Storable VkDisplayModePropertiesKHR where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr = VkDisplayModePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplayMode (poked :: VkDisplayModePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkParameters (poked :: VkDisplayModePropertiesKHR))



data VkDisplayModeCreateInfoKHR =
  VkDisplayModeCreateInfoKHR{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkFlags :: VkDisplayModeCreateFlagsKHR 
                            , vkParameters :: VkDisplayModeParametersKHR 
                            }
  deriving (Eq)

instance Storable VkDisplayModeCreateInfoKHR where
  sizeOf _ = 32
  alignment _ = 8
  peek ptr = VkDisplayModeCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkParameters (poked :: VkDisplayModeCreateInfoKHR))



data VkDisplayPlaneCapabilitiesKHR =
  VkDisplayPlaneCapabilitiesKHR{ vkSupportedAlpha :: VkDisplayPlaneAlphaFlagsKHR 
                               , vkMinSrcPosition :: VkOffset2D 
                               , vkMaxSrcPosition :: VkOffset2D 
                               , vkMinSrcExtent :: VkExtent2D 
                               , vkMaxSrcExtent :: VkExtent2D 
                               , vkMinDstPosition :: VkOffset2D 
                               , vkMaxDstPosition :: VkOffset2D 
                               , vkMinDstExtent :: VkExtent2D 
                               , vkMaxDstExtent :: VkExtent2D 
                               }
  deriving (Eq)

instance Storable VkDisplayPlaneCapabilitiesKHR where
  sizeOf _ = 68
  alignment _ = 4
  peek ptr = VkDisplayPlaneCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 12)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 28)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 52)
                                           <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSupportedAlpha (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMinSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 12) (vkMaxSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 20) (vkMinSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 28) (vkMaxSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkMinDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkMaxDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 52) (vkMinDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 60) (vkMaxDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))



data VkDisplaySurfaceCreateInfoKHR =
  VkDisplaySurfaceCreateInfoKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkFlags :: VkDisplaySurfaceCreateFlagsKHR 
                               , vkDisplayMode :: VkDisplayModeKHR 
                               , vkPlaneIndex :: Word32 
                               , vkPlaneStackIndex :: Word32 
                               , vkTransform :: VkSurfaceTransformFlagBitsKHR 
                               , vkGlobalAlpha :: CFloat 
                               , vkAlphaMode :: VkDisplayPlaneAlphaFlagBitsKHR 
                               , vkImageExtent :: VkExtent2D 
                               }
  deriving (Eq)

instance Storable VkDisplaySurfaceCreateInfoKHR where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkDisplaySurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 40)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 48)
                                           <*> peek (ptr `plusPtr` 52)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDisplayMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkPlaneIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneStackIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkTransform (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkGlobalAlpha (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkAlphaMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageExtent (poked :: VkDisplaySurfaceCreateInfoKHR))



data VkDisplayPresentInfoKHR =
  VkDisplayPresentInfoKHR{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkSrcRect :: VkRect2D 
                         , vkDstRect :: VkRect2D 
                         , vkPersistent :: VkBool32 
                         }
  deriving (Eq)

instance Storable VkDisplayPresentInfoKHR where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr = VkDisplayPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSrcRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkDstRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPersistent (poked :: VkDisplayPresentInfoKHR))



data VkSurfaceCapabilitiesKHR =
  VkSurfaceCapabilitiesKHR{ vkMinImageCount :: Word32 
                          , vkMaxImageCount :: Word32 
                          , vkCurrentExtent :: VkExtent2D 
                          , vkMinImageExtent :: VkExtent2D 
                          , vkMaxImageExtent :: VkExtent2D 
                          , vkMaxImageArrayLayers :: Word32 
                          , vkSupportedTransforms :: VkSurfaceTransformFlagsKHR 
                          , vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR 
                          , vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR 
                          , vkSupportedUsageFlags :: VkImageUsageFlags 
                          }
  deriving (Eq)

instance Storable VkSurfaceCapabilitiesKHR where
  sizeOf _ = 52
  alignment _ = 4
  peek ptr = VkSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMinImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMaxImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMinImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 24) (vkMaxImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 32) (vkMaxImageArrayLayers (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkSupportedTransforms (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 40) (vkCurrentTransform (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkSupportedCompositeAlpha (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 48) (vkSupportedUsageFlags (poked :: VkSurfaceCapabilitiesKHR))



data VkSurfaceFormatKHR =
  VkSurfaceFormatKHR{ vkFormat :: VkFormat 
                    , vkColorSpace :: VkColorSpaceKHR 
                    }
  deriving (Eq)

instance Storable VkSurfaceFormatKHR where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = VkSurfaceFormatKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFormat (poked :: VkSurfaceFormatKHR))
                *> poke (ptr `plusPtr` 4) (vkColorSpace (poked :: VkSurfaceFormatKHR))



data VkSwapchainCreateInfoKHR =
  VkSwapchainCreateInfoKHR{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkSwapchainCreateFlagsKHR 
                          , vkSurface :: VkSurfaceKHR 
                          , vkMinImageCount :: Word32 
                          , vkImageFormat :: VkFormat 
                          , vkImageColorSpace :: VkColorSpaceKHR 
                          , vkImageExtent :: VkExtent2D 
                          , vkImageArrayLayers :: Word32 
                          , vkImageUsage :: VkImageUsageFlags 
                          , vkImageSharingMode :: VkSharingMode 
                          , vkQueueFamilyIndexCount :: Word32 
                          , vkPQueueFamilyIndices :: Ptr Word32 
                          , vkPreTransform :: VkSurfaceTransformFlagBitsKHR 
                          , vkCompositeAlpha :: VkCompositeAlphaFlagBitsKHR 
                          , vkPresentMode :: VkPresentModeKHR 
                          , vkClipped :: VkBool32 
                          , vkOldSwapchain :: VkSwapchainKHR 
                          }
  deriving (Eq)

instance Storable VkSwapchainCreateInfoKHR where
  sizeOf _ = 104
  alignment _ = 8
  peek ptr = VkSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 60)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 80)
                                      <*> peek (ptr `plusPtr` 84)
                                      <*> peek (ptr `plusPtr` 88)
                                      <*> peek (ptr `plusPtr` 92)
                                      <*> peek (ptr `plusPtr` 96)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkSurface (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMinImageCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkImageFormat (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkImageColorSpace (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageArrayLayers (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkImageUsage (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 60) (vkImageSharingMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (vkPreTransform (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (vkCompositeAlpha (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (vkPresentMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (vkClipped (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (vkOldSwapchain (poked :: VkSwapchainCreateInfoKHR))



data VkPresentInfoKHR =
  VkPresentInfoKHR{ vkSType :: VkStructureType 
                  , vkPNext :: Ptr Void 
                  , vkWaitSemaphoreCount :: Word32 
                  , vkPWaitSemaphores :: Ptr VkSemaphore 
                  , vkSwapchainCount :: Word32 
                  , vkPSwapchains :: Ptr VkSwapchainKHR 
                  , vkPImageIndices :: Ptr Word32 
                  , vkPResults :: Ptr VkResult 
                  }
  deriving (Eq)

instance Storable VkPresentInfoKHR where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = VkPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSwapchainCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPSwapchains (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPImageIndices (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPResults (poked :: VkPresentInfoKHR))



data VkDebugReportCallbackCreateInfoEXT =
  VkDebugReportCallbackCreateInfoEXT{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkFlags :: VkDebugReportFlagsEXT 
                                    , vkPfnCallback :: PFN_vkDebugReportCallbackEXT 
                                    , vkPUserData :: Ptr Void 
                                    }
  deriving (Eq)

instance Storable VkDebugReportCallbackCreateInfoEXT where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = VkDebugReportCallbackCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPfnCallback (poked :: VkDebugReportCallbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPUserData (poked :: VkDebugReportCallbackCreateInfoEXT))


-- * Union Types

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
data VkClearColorValue = VkFloat (Vec (ToPeano 4) CFloat) 
                       | VkInt (Vec (ToPeano 4) Int32) 
                       | VkUint (Vec (ToPeano 4) Word32) 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearColorValue where
  sizeOf _ = 16
  alignment _ = 4
  peek _ = error "peek@VkClearColorValue"
  poke ptr poked = case poked of
                     VkFloat e -> poke (castPtr ptr) e
                     VkInt e -> poke (castPtr ptr) e
                     VkUint e -> poke (castPtr ptr) e


-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
data VkClearValue = VkColor VkClearColorValue 
                  | VkDepthStencil VkClearDepthStencilValue 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearValue where
  sizeOf _ = 16
  alignment _ = 4
  peek _ = error "peek@VkClearValue"
  poke ptr poked = case poked of
                     VkColor e -> poke (castPtr ptr) e
                     VkDepthStencil e -> poke (castPtr ptr) e


-- * Commands

-- ** vkCreateInstance
foreign import ccall "vkCreateInstance" vkCreateInstance :: 
  Ptr VkInstanceCreateInfo ->
  Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult

-- ** vkDestroyInstance
foreign import ccall "vkDestroyInstance" vkDestroyInstance :: 
  VkInstance -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkEnumeratePhysicalDevices
foreign import ccall "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices :: 
  VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult

-- ** vkGetDeviceProcAddr
foreign import ccall "vkGetDeviceProcAddr" vkGetDeviceProcAddr :: 
  VkDevice -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** vkGetInstanceProcAddr
foreign import ccall "vkGetInstanceProcAddr" vkGetInstanceProcAddr :: 
  VkInstance -> Ptr CChar -> IO PFN_vkVoidFunction

-- ** vkGetPhysicalDeviceProperties
foreign import ccall "vkGetPhysicalDeviceProperties" vkGetPhysicalDeviceProperties :: 
  VkPhysicalDevice -> Ptr VkPhysicalDeviceProperties -> IO ()

-- ** vkGetPhysicalDeviceQueueFamilyProperties
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties :: 
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkQueueFamilyProperties -> IO ()

-- ** vkGetPhysicalDeviceMemoryProperties
foreign import ccall "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties :: 
  VkPhysicalDevice -> Ptr VkPhysicalDeviceMemoryProperties -> IO ()

-- ** vkGetPhysicalDeviceFeatures
foreign import ccall "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures :: 
  VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ()

-- ** vkGetPhysicalDeviceFormatProperties
foreign import ccall "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties :: 
  VkPhysicalDevice -> VkFormat -> Ptr VkFormatProperties -> IO ()

-- ** vkGetPhysicalDeviceImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties :: 
  VkPhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkImageTiling ->
        VkImageUsageFlags ->
          VkImageCreateFlags -> Ptr VkImageFormatProperties -> IO VkResult

-- ** vkCreateDevice
foreign import ccall "vkCreateDevice" vkCreateDevice :: 
  VkPhysicalDevice ->
  Ptr VkDeviceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkDevice -> IO VkResult

-- ** vkDestroyDevice
foreign import ccall "vkDestroyDevice" vkDestroyDevice :: 
  VkDevice -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkEnumerateInstanceLayerProperties
foreign import ccall "vkEnumerateInstanceLayerProperties" vkEnumerateInstanceLayerProperties :: 
  Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult

-- ** vkEnumerateInstanceExtensionProperties
foreign import ccall "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: 
  Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult

-- ** vkEnumerateDeviceLayerProperties
foreign import ccall "vkEnumerateDeviceLayerProperties" vkEnumerateDeviceLayerProperties :: 
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult

-- ** vkEnumerateDeviceExtensionProperties
foreign import ccall "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: 
  VkPhysicalDevice ->
  Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult

-- ** vkGetDeviceQueue
foreign import ccall "vkGetDeviceQueue" vkGetDeviceQueue :: 
  VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()

-- ** vkQueueSubmit
foreign import ccall "vkQueueSubmit" vkQueueSubmit :: 
  VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult

-- ** vkQueueWaitIdle
foreign import ccall "vkQueueWaitIdle" vkQueueWaitIdle :: 
  VkQueue -> IO VkResult

-- ** vkDeviceWaitIdle
foreign import ccall "vkDeviceWaitIdle" vkDeviceWaitIdle :: 
  VkDevice -> IO VkResult

-- ** vkAllocateMemory
foreign import ccall "vkAllocateMemory" vkAllocateMemory :: 
  VkDevice ->
  Ptr VkMemoryAllocateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkDeviceMemory -> IO VkResult

-- ** vkFreeMemory
foreign import ccall "vkFreeMemory" vkFreeMemory :: 
  VkDevice -> VkDeviceMemory -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkMapMemory
foreign import ccall "vkMapMemory" vkMapMemory :: 
  VkDevice ->
  VkDeviceMemory ->
    VkDeviceSize ->
      VkDeviceSize -> VkMemoryMapFlags -> Ptr (Ptr Void) -> IO VkResult

-- ** vkUnmapMemory
foreign import ccall "vkUnmapMemory" vkUnmapMemory :: 
  VkDevice -> VkDeviceMemory -> IO ()

-- ** vkFlushMappedMemoryRanges
foreign import ccall "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges :: 
  VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult

-- ** vkInvalidateMappedMemoryRanges
foreign import ccall "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges :: 
  VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult

-- ** vkGetDeviceMemoryCommitment
foreign import ccall "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment :: 
  VkDevice -> VkDeviceMemory -> Ptr VkDeviceSize -> IO ()

-- ** vkGetBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements :: 
  VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()

-- ** vkBindBufferMemory
foreign import ccall "vkBindBufferMemory" vkBindBufferMemory :: 
  VkDevice ->
  VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

-- ** vkGetImageMemoryRequirements
foreign import ccall "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements :: 
  VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()

-- ** vkBindImageMemory
foreign import ccall "vkBindImageMemory" vkBindImageMemory :: 
  VkDevice ->
  VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

-- ** vkGetImageSparseMemoryRequirements
foreign import ccall "vkGetImageSparseMemoryRequirements" vkGetImageSparseMemoryRequirements :: 
  VkDevice ->
  VkImage ->
    Ptr Word32 -> Ptr VkSparseImageMemoryRequirements -> IO ()

-- ** vkGetPhysicalDeviceSparseImageFormatProperties
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties" vkGetPhysicalDeviceSparseImageFormatProperties :: 
  VkPhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkSampleCountFlagBits ->
        VkImageUsageFlags ->
          VkImageTiling ->
            Ptr Word32 -> Ptr VkSparseImageFormatProperties -> IO ()

-- ** vkQueueBindSparse
foreign import ccall "vkQueueBindSparse" vkQueueBindSparse :: 
  VkQueue -> Word32 -> Ptr VkBindSparseInfo -> VkFence -> IO VkResult

-- ** vkCreateFence
foreign import ccall "vkCreateFence" vkCreateFence :: 
  VkDevice ->
  Ptr VkFenceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult

-- ** vkDestroyFence
foreign import ccall "vkDestroyFence" vkDestroyFence :: 
  VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkResetFences
foreign import ccall "vkResetFences" vkResetFences :: 
  VkDevice -> Word32 -> Ptr VkFence -> IO VkResult

-- ** vkGetFenceStatus
foreign import ccall "vkGetFenceStatus" vkGetFenceStatus :: 
  VkDevice -> VkFence -> IO VkResult

-- ** vkWaitForFences
foreign import ccall "vkWaitForFences" vkWaitForFences :: 
  VkDevice ->
  Word32 -> Ptr VkFence -> VkBool32 -> Word64 -> IO VkResult

-- ** vkCreateSemaphore
foreign import ccall "vkCreateSemaphore" vkCreateSemaphore :: 
  VkDevice ->
  Ptr VkSemaphoreCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkSemaphore -> IO VkResult

-- ** vkDestroySemaphore
foreign import ccall "vkDestroySemaphore" vkDestroySemaphore :: 
  VkDevice -> VkSemaphore -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateEvent
foreign import ccall "vkCreateEvent" vkCreateEvent :: 
  VkDevice ->
  Ptr VkEventCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkEvent -> IO VkResult

-- ** vkDestroyEvent
foreign import ccall "vkDestroyEvent" vkDestroyEvent :: 
  VkDevice -> VkEvent -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetEventStatus
foreign import ccall "vkGetEventStatus" vkGetEventStatus :: 
  VkDevice -> VkEvent -> IO VkResult

-- ** vkSetEvent
foreign import ccall "vkSetEvent" vkSetEvent :: 
  VkDevice -> VkEvent -> IO VkResult

-- ** vkResetEvent
foreign import ccall "vkResetEvent" vkResetEvent :: 
  VkDevice -> VkEvent -> IO VkResult

-- ** vkCreateQueryPool
foreign import ccall "vkCreateQueryPool" vkCreateQueryPool :: 
  VkDevice ->
  Ptr VkQueryPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkQueryPool -> IO VkResult

-- ** vkDestroyQueryPool
foreign import ccall "vkDestroyQueryPool" vkDestroyQueryPool :: 
  VkDevice -> VkQueryPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetQueryPoolResults
foreign import ccall "vkGetQueryPoolResults" vkGetQueryPoolResults :: 
  VkDevice ->
  VkQueryPool ->
    Word32 ->
      Word32 ->
        CSize ->
          Ptr Void -> VkDeviceSize -> VkQueryResultFlags -> IO VkResult

-- ** vkCreateBuffer
foreign import ccall "vkCreateBuffer" vkCreateBuffer :: 
  VkDevice ->
  Ptr VkBufferCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkBuffer -> IO VkResult

-- ** vkDestroyBuffer
foreign import ccall "vkDestroyBuffer" vkDestroyBuffer :: 
  VkDevice -> VkBuffer -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateBufferView
foreign import ccall "vkCreateBufferView" vkCreateBufferView :: 
  VkDevice ->
  Ptr VkBufferViewCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkBufferView -> IO VkResult

-- ** vkDestroyBufferView
foreign import ccall "vkDestroyBufferView" vkDestroyBufferView :: 
  VkDevice -> VkBufferView -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateImage
foreign import ccall "vkCreateImage" vkCreateImage :: 
  VkDevice ->
  Ptr VkImageCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult

-- ** vkDestroyImage
foreign import ccall "vkDestroyImage" vkDestroyImage :: 
  VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetImageSubresourceLayout
foreign import ccall "vkGetImageSubresourceLayout" vkGetImageSubresourceLayout :: 
  VkDevice ->
  VkImage ->
    Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()

-- ** vkCreateImageView
foreign import ccall "vkCreateImageView" vkCreateImageView :: 
  VkDevice ->
  Ptr VkImageViewCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult

-- ** vkDestroyImageView
foreign import ccall "vkDestroyImageView" vkDestroyImageView :: 
  VkDevice -> VkImageView -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateShaderModule
foreign import ccall "vkCreateShaderModule" vkCreateShaderModule :: 
  VkDevice ->
  Ptr VkShaderModuleCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkShaderModule -> IO VkResult

-- ** vkDestroyShaderModule
foreign import ccall "vkDestroyShaderModule" vkDestroyShaderModule :: 
  VkDevice -> VkShaderModule -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreatePipelineCache
foreign import ccall "vkCreatePipelineCache" vkCreatePipelineCache :: 
  VkDevice ->
  Ptr VkPipelineCacheCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkPipelineCache -> IO VkResult

-- ** vkDestroyPipelineCache
foreign import ccall "vkDestroyPipelineCache" vkDestroyPipelineCache :: 
  VkDevice -> VkPipelineCache -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetPipelineCacheData
foreign import ccall "vkGetPipelineCacheData" vkGetPipelineCacheData :: 
  VkDevice -> VkPipelineCache -> Ptr CSize -> Ptr Void -> IO VkResult

-- ** vkMergePipelineCaches
foreign import ccall "vkMergePipelineCaches" vkMergePipelineCaches :: 
  VkDevice ->
  VkPipelineCache -> Word32 -> Ptr VkPipelineCache -> IO VkResult

-- ** vkCreateGraphicsPipelines
foreign import ccall "vkCreateGraphicsPipelines" vkCreateGraphicsPipelines :: 
  VkDevice ->
  VkPipelineCache ->
    Word32 ->
      Ptr VkGraphicsPipelineCreateInfo ->
        Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult

-- ** vkCreateComputePipelines
foreign import ccall "vkCreateComputePipelines" vkCreateComputePipelines :: 
  VkDevice ->
  VkPipelineCache ->
    Word32 ->
      Ptr VkComputePipelineCreateInfo ->
        Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult

-- ** vkDestroyPipeline
foreign import ccall "vkDestroyPipeline" vkDestroyPipeline :: 
  VkDevice -> VkPipeline -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreatePipelineLayout
foreign import ccall "vkCreatePipelineLayout" vkCreatePipelineLayout :: 
  VkDevice ->
  Ptr VkPipelineLayoutCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkPipelineLayout -> IO VkResult

-- ** vkDestroyPipelineLayout
foreign import ccall "vkDestroyPipelineLayout" vkDestroyPipelineLayout :: 
  VkDevice -> VkPipelineLayout -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateSampler
foreign import ccall "vkCreateSampler" vkCreateSampler :: 
  VkDevice ->
  Ptr VkSamplerCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkSampler -> IO VkResult

-- ** vkDestroySampler
foreign import ccall "vkDestroySampler" vkDestroySampler :: 
  VkDevice -> VkSampler -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateDescriptorSetLayout
foreign import ccall "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout :: 
  VkDevice ->
  Ptr VkDescriptorSetLayoutCreateInfo ->
    Ptr VkAllocationCallbacks ->
      Ptr VkDescriptorSetLayout -> IO VkResult

-- ** vkDestroyDescriptorSetLayout
foreign import ccall "vkDestroyDescriptorSetLayout" vkDestroyDescriptorSetLayout :: 
  VkDevice ->
  VkDescriptorSetLayout -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateDescriptorPool
foreign import ccall "vkCreateDescriptorPool" vkCreateDescriptorPool :: 
  VkDevice ->
  Ptr VkDescriptorPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkDescriptorPool -> IO VkResult

-- ** vkDestroyDescriptorPool
foreign import ccall "vkDestroyDescriptorPool" vkDestroyDescriptorPool :: 
  VkDevice -> VkDescriptorPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkResetDescriptorPool
foreign import ccall "vkResetDescriptorPool" vkResetDescriptorPool :: 
  VkDevice ->
  VkDescriptorPool -> VkDescriptorPoolResetFlags -> IO VkResult

-- ** vkAllocateDescriptorSets
foreign import ccall "vkAllocateDescriptorSets" vkAllocateDescriptorSets :: 
  VkDevice ->
  Ptr VkDescriptorSetAllocateInfo ->
    Ptr VkDescriptorSet -> IO VkResult

-- ** vkFreeDescriptorSets
foreign import ccall "vkFreeDescriptorSets" vkFreeDescriptorSets :: 
  VkDevice ->
  VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO VkResult

-- ** vkUpdateDescriptorSets
foreign import ccall "vkUpdateDescriptorSets" vkUpdateDescriptorSets :: 
  VkDevice ->
  Word32 ->
    Ptr VkWriteDescriptorSet ->
      Word32 -> Ptr VkCopyDescriptorSet -> IO ()

-- ** vkCreateFramebuffer
foreign import ccall "vkCreateFramebuffer" vkCreateFramebuffer :: 
  VkDevice ->
  Ptr VkFramebufferCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkFramebuffer -> IO VkResult

-- ** vkDestroyFramebuffer
foreign import ccall "vkDestroyFramebuffer" vkDestroyFramebuffer :: 
  VkDevice -> VkFramebuffer -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateRenderPass
foreign import ccall "vkCreateRenderPass" vkCreateRenderPass :: 
  VkDevice ->
  Ptr VkRenderPassCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkRenderPass -> IO VkResult

-- ** vkDestroyRenderPass
foreign import ccall "vkDestroyRenderPass" vkDestroyRenderPass :: 
  VkDevice -> VkRenderPass -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetRenderAreaGranularity
foreign import ccall "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity :: 
  VkDevice -> VkRenderPass -> Ptr VkExtent2D -> IO ()

-- ** vkCreateCommandPool
foreign import ccall "vkCreateCommandPool" vkCreateCommandPool :: 
  VkDevice ->
  Ptr VkCommandPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkCommandPool -> IO VkResult

-- ** vkDestroyCommandPool
foreign import ccall "vkDestroyCommandPool" vkDestroyCommandPool :: 
  VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkResetCommandPool
foreign import ccall "vkResetCommandPool" vkResetCommandPool :: 
  VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> IO VkResult

-- ** vkAllocateCommandBuffers
foreign import ccall "vkAllocateCommandBuffers" vkAllocateCommandBuffers :: 
  VkDevice ->
  Ptr VkCommandBufferAllocateInfo ->
    Ptr VkCommandBuffer -> IO VkResult

-- ** vkFreeCommandBuffers
foreign import ccall "vkFreeCommandBuffers" vkFreeCommandBuffers :: 
  VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()

-- ** vkBeginCommandBuffer
foreign import ccall "vkBeginCommandBuffer" vkBeginCommandBuffer :: 
  VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult

-- ** vkEndCommandBuffer
foreign import ccall "vkEndCommandBuffer" vkEndCommandBuffer :: 
  VkCommandBuffer -> IO VkResult

-- ** vkResetCommandBuffer
foreign import ccall "vkResetCommandBuffer" vkResetCommandBuffer :: 
  VkCommandBuffer -> VkCommandBufferResetFlags -> IO VkResult

-- ** vkCmdBindPipeline
foreign import ccall "vkCmdBindPipeline" vkCmdBindPipeline :: 
  VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()

-- ** vkCmdSetViewport
foreign import ccall "vkCmdSetViewport" vkCmdSetViewport :: 
  VkCommandBuffer -> Word32 -> Word32 -> Ptr VkViewport -> IO ()

-- ** vkCmdSetScissor
foreign import ccall "vkCmdSetScissor" vkCmdSetScissor :: 
  VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()

-- ** vkCmdSetLineWidth
foreign import ccall "vkCmdSetLineWidth" vkCmdSetLineWidth :: 
  VkCommandBuffer -> CFloat -> IO ()

-- ** vkCmdSetDepthBias
foreign import ccall "vkCmdSetDepthBias" vkCmdSetDepthBias :: 
  VkCommandBuffer -> CFloat -> CFloat -> CFloat -> IO ()

-- ** vkCmdSetBlendConstants
foreign import ccall "vkCmdSetBlendConstants" vkCmdSetBlendConstants :: 
  VkCommandBuffer -> Ptr CFloat -> IO ()

-- ** vkCmdSetDepthBounds
foreign import ccall "vkCmdSetDepthBounds" vkCmdSetDepthBounds :: 
  VkCommandBuffer -> CFloat -> CFloat -> IO ()

-- ** vkCmdSetStencilCompareMask
foreign import ccall "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask :: 
  VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdSetStencilWriteMask
foreign import ccall "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask :: 
  VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdSetStencilReference
foreign import ccall "vkCmdSetStencilReference" vkCmdSetStencilReference :: 
  VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdBindDescriptorSets
foreign import ccall "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets :: 
  VkCommandBuffer ->
  VkPipelineBindPoint ->
    VkPipelineLayout ->
      Word32 ->
        Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- ** vkCmdBindIndexBuffer
foreign import ccall "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer :: 
  VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkIndexType -> IO ()

-- ** vkCmdBindVertexBuffers
foreign import ccall "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers :: 
  VkCommandBuffer ->
  Word32 -> Word32 -> Ptr VkBuffer -> Ptr VkDeviceSize -> IO ()

-- ** vkCmdDraw
foreign import ccall "vkCmdDraw" vkCmdDraw :: 
  VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- ** vkCmdDrawIndexed
foreign import ccall "vkCmdDrawIndexed" vkCmdDrawIndexed :: 
  VkCommandBuffer ->
  Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- ** vkCmdDrawIndirect
foreign import ccall "vkCmdDrawIndirect" vkCmdDrawIndirect :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()

-- ** vkCmdDrawIndexedIndirect
foreign import ccall "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()

-- ** vkCmdDispatch
foreign import ccall "vkCmdDispatch" vkCmdDispatch :: 
  VkCommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()

-- ** vkCmdDispatchIndirect
foreign import ccall "vkCmdDispatchIndirect" vkCmdDispatchIndirect :: 
  VkCommandBuffer -> VkBuffer -> VkDeviceSize -> IO ()

-- ** vkCmdCopyBuffer
foreign import ccall "vkCmdCopyBuffer" vkCmdCopyBuffer :: 
  VkCommandBuffer ->
  VkBuffer -> VkBuffer -> Word32 -> Ptr VkBufferCopy -> IO ()

-- ** vkCmdCopyImage
foreign import ccall "vkCmdCopyImage" vkCmdCopyImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkImage -> VkImageLayout -> Word32 -> Ptr VkImageCopy -> IO ()

-- ** vkCmdBlitImage
foreign import ccall "vkCmdBlitImage" vkCmdBlitImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkImage ->
        VkImageLayout -> Word32 -> Ptr VkImageBlit -> VkFilter -> IO ()

-- ** vkCmdCopyBufferToImage
foreign import ccall "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage :: 
  VkCommandBuffer ->
  VkBuffer ->
    VkImage ->
      VkImageLayout -> Word32 -> Ptr VkBufferImageCopy -> IO ()

-- ** vkCmdCopyImageToBuffer
foreign import ccall "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkBuffer -> Word32 -> Ptr VkBufferImageCopy -> IO ()

-- ** vkCmdUpdateBuffer
foreign import ccall "vkCmdUpdateBuffer" vkCmdUpdateBuffer :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> VkDeviceSize -> Ptr Word32 -> IO ()

-- ** vkCmdFillBuffer
foreign import ccall "vkCmdFillBuffer" vkCmdFillBuffer :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()

-- ** vkCmdClearColorImage
foreign import ccall "vkCmdClearColorImage" vkCmdClearColorImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      Ptr VkClearColorValue ->
        Word32 -> Ptr VkImageSubresourceRange -> IO ()

-- ** vkCmdClearDepthStencilImage
foreign import ccall "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      Ptr VkClearDepthStencilValue ->
        Word32 -> Ptr VkImageSubresourceRange -> IO ()

-- ** vkCmdClearAttachments
foreign import ccall "vkCmdClearAttachments" vkCmdClearAttachments :: 
  VkCommandBuffer ->
  Word32 ->
    Ptr VkClearAttachment -> Word32 -> Ptr VkClearRect -> IO ()

-- ** vkCmdResolveImage
foreign import ccall "vkCmdResolveImage" vkCmdResolveImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkImage -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ()

-- ** vkCmdSetEvent
foreign import ccall "vkCmdSetEvent" vkCmdSetEvent :: 
  VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()

-- ** vkCmdResetEvent
foreign import ccall "vkCmdResetEvent" vkCmdResetEvent :: 
  VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()

-- ** vkCmdWaitEvents
foreign import ccall "vkCmdWaitEvents" vkCmdWaitEvents :: 
  VkCommandBuffer ->
  Word32 ->
    Ptr VkEvent ->
      VkPipelineStageFlags ->
        VkPipelineStageFlags ->
          Word32 ->
            Ptr VkMemoryBarrier ->
              Word32 ->
                Ptr VkBufferMemoryBarrier ->
                  Word32 -> Ptr VkImageMemoryBarrier -> IO ()

-- ** vkCmdPipelineBarrier
foreign import ccall "vkCmdPipelineBarrier" vkCmdPipelineBarrier :: 
  VkCommandBuffer ->
  VkPipelineStageFlags ->
    VkPipelineStageFlags ->
      VkDependencyFlags ->
        Word32 ->
          Ptr VkMemoryBarrier ->
            Word32 ->
              Ptr VkBufferMemoryBarrier ->
                Word32 -> Ptr VkImageMemoryBarrier -> IO ()

-- ** vkCmdBeginQuery
foreign import ccall "vkCmdBeginQuery" vkCmdBeginQuery :: 
  VkCommandBuffer ->
  VkQueryPool -> Word32 -> VkQueryControlFlags -> IO ()

-- ** vkCmdEndQuery
foreign import ccall "vkCmdEndQuery" vkCmdEndQuery :: 
  VkCommandBuffer -> VkQueryPool -> Word32 -> IO ()

-- ** vkCmdResetQueryPool
foreign import ccall "vkCmdResetQueryPool" vkCmdResetQueryPool :: 
  VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()

-- ** vkCmdWriteTimestamp
foreign import ccall "vkCmdWriteTimestamp" vkCmdWriteTimestamp :: 
  VkCommandBuffer ->
  VkPipelineStageFlagBits -> VkQueryPool -> Word32 -> IO ()

-- ** vkCmdCopyQueryPoolResults
foreign import ccall "vkCmdCopyQueryPoolResults" vkCmdCopyQueryPoolResults :: 
  VkCommandBuffer ->
  VkQueryPool ->
    Word32 ->
      Word32 ->
        VkBuffer ->
          VkDeviceSize -> VkDeviceSize -> VkQueryResultFlags -> IO ()

-- ** vkCmdPushConstants
foreign import ccall "vkCmdPushConstants" vkCmdPushConstants :: 
  VkCommandBuffer ->
  VkPipelineLayout ->
    VkShaderStageFlags -> Word32 -> Word32 -> Ptr Void -> IO ()

-- ** vkCmdBeginRenderPass
foreign import ccall "vkCmdBeginRenderPass" vkCmdBeginRenderPass :: 
  VkCommandBuffer ->
  Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()

-- ** vkCmdNextSubpass
foreign import ccall "vkCmdNextSubpass" vkCmdNextSubpass :: 
  VkCommandBuffer -> VkSubpassContents -> IO ()

-- ** vkCmdEndRenderPass
foreign import ccall "vkCmdEndRenderPass" vkCmdEndRenderPass :: 
  VkCommandBuffer -> IO ()

-- ** vkCmdExecuteCommands
foreign import ccall "vkCmdExecuteCommands" vkCmdExecuteCommands :: 
  VkCommandBuffer -> Word32 -> Ptr VkCommandBuffer -> IO ()

-- ** vkGetPhysicalDeviceDisplayPropertiesKHR
foreign import ccall "vkGetPhysicalDeviceDisplayPropertiesKHR" vkGetPhysicalDeviceDisplayPropertiesKHR :: 
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult

-- ** vkGetPhysicalDeviceDisplayPlanePropertiesKHR
foreign import ccall "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: 
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult

-- ** vkGetDisplayPlaneSupportedDisplaysKHR
foreign import ccall "vkGetDisplayPlaneSupportedDisplaysKHR" vkGetDisplayPlaneSupportedDisplaysKHR :: 
  VkPhysicalDevice ->
  Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult

-- ** vkGetDisplayModePropertiesKHR
foreign import ccall "vkGetDisplayModePropertiesKHR" vkGetDisplayModePropertiesKHR :: 
  VkPhysicalDevice ->
  VkDisplayKHR ->
    Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult

-- ** vkCreateDisplayModeKHR
foreign import ccall "vkCreateDisplayModeKHR" vkCreateDisplayModeKHR :: 
  VkPhysicalDevice ->
  VkDisplayKHR ->
    Ptr VkDisplayModeCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult

-- ** vkGetDisplayPlaneCapabilitiesKHR
foreign import ccall "vkGetDisplayPlaneCapabilitiesKHR" vkGetDisplayPlaneCapabilitiesKHR :: 
  VkPhysicalDevice ->
  VkDisplayModeKHR ->
    Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult

-- ** vkCreateDisplayPlaneSurfaceKHR
foreign import ccall "vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR :: 
  VkInstance ->
  Ptr VkDisplaySurfaceCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult

-- ** vkCreateSharedSwapchainsKHR
foreign import ccall "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR :: 
  VkDevice ->
  Word32 ->
    Ptr VkSwapchainCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult

-- ** vkDestroySurfaceKHR
foreign import ccall "vkDestroySurfaceKHR" vkDestroySurfaceKHR :: 
  VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetPhysicalDeviceSurfaceSupportKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" vkGetPhysicalDeviceSurfaceSupportKHR :: 
  VkPhysicalDevice ->
  Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult

-- ** vkGetPhysicalDeviceSurfaceCapabilitiesKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: 
  VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult

-- ** vkGetPhysicalDeviceSurfaceFormatsKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR" vkGetPhysicalDeviceSurfaceFormatsKHR :: 
  VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult

-- ** vkGetPhysicalDeviceSurfacePresentModesKHR
foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR" vkGetPhysicalDeviceSurfacePresentModesKHR :: 
  VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult

-- ** vkCreateSwapchainKHR
foreign import ccall "vkCreateSwapchainKHR" vkCreateSwapchainKHR :: 
  VkDevice ->
  Ptr VkSwapchainCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult

-- ** vkDestroySwapchainKHR
foreign import ccall "vkDestroySwapchainKHR" vkDestroySwapchainKHR :: 
  VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkGetSwapchainImagesKHR
foreign import ccall "vkGetSwapchainImagesKHR" vkGetSwapchainImagesKHR :: 
  VkDevice ->
  VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult

-- ** vkAcquireNextImageKHR
foreign import ccall "vkAcquireNextImageKHR" vkAcquireNextImageKHR :: 
  VkDevice ->
  VkSwapchainKHR ->
    Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult

-- ** vkQueuePresentKHR
foreign import ccall "vkQueuePresentKHR" vkQueuePresentKHR :: 
  VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult

-- ** vkGetPhysicalDeviceWin32PresentationSupportKHR
foreign import ccall "vkGetPhysicalDeviceWin32PresentationSupportKHR" vkGetPhysicalDeviceWin32PresentationSupportKHR :: 
  VkPhysicalDevice -> Word32 -> IO VkBool32

-- ** vkCreateDebugReportCallbackEXT
foreign import ccall "vkCreateDebugReportCallbackEXT" vkCreateDebugReportCallbackEXT :: 
  VkInstance ->
  Ptr VkDebugReportCallbackCreateInfoEXT ->
    Ptr VkAllocationCallbacks ->
      Ptr VkDebugReportCallbackEXT -> IO VkResult

-- ** vkDestroyDebugReportCallbackEXT
foreign import ccall "vkDestroyDebugReportCallbackEXT" vkDestroyDebugReportCallbackEXT :: 
  VkInstance ->
  VkDebugReportCallbackEXT -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkDebugReportMessageEXT
foreign import ccall "vkDebugReportMessageEXT" vkDebugReportMessageEXT :: 
  VkInstance ->
  VkDebugReportFlagsEXT ->
    VkDebugReportObjectTypeEXT ->
      Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()


