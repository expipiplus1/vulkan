{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageTiling(..)
  , pattern VK_IMAGE_TILING_OPTIMAL
  , pattern VK_IMAGE_TILING_LINEAR
  , VkImageType(..)
  , pattern VK_IMAGE_TYPE_1D
  , pattern VK_IMAGE_TYPE_2D
  , pattern VK_IMAGE_TYPE_3D
  , VkPhysicalDeviceType(..)
  , pattern VK_PHYSICAL_DEVICE_TYPE_OTHER
  , pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_CPU
  , VkSystemAllocationScope(..)
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
  , VkInternalAllocationType(..)
  , pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
  , VkInstanceCreateFlags(..)
  , VkQueueFlagBits(..)
  , pattern VK_QUEUE_GRAPHICS_BIT
  , pattern VK_QUEUE_COMPUTE_BIT
  , pattern VK_QUEUE_TRANSFER_BIT
  , pattern VK_QUEUE_SPARSE_BINDING_BIT
  , VkMemoryPropertyFlagBits(..)
  , pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , VkMemoryHeapFlagBits(..)
  , pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
  , VkImageUsageFlagBits(..)
  , pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_USAGE_SAMPLED_BIT
  , pattern VK_IMAGE_USAGE_STORAGE_BIT
  , pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , VkImageCreateFlagBits(..)
  , pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , VkFormatFeatureFlagBits(..)
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
  , pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT
  , pattern VK_FORMAT_FEATURE_BLIT_DST_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  , VkSampleCountFlagBits(..)
  , pattern VK_SAMPLE_COUNT_1_BIT
  , pattern VK_SAMPLE_COUNT_2_BIT
  , pattern VK_SAMPLE_COUNT_4_BIT
  , pattern VK_SAMPLE_COUNT_8_BIT
  , pattern VK_SAMPLE_COUNT_16_BIT
  , pattern VK_SAMPLE_COUNT_32_BIT
  , pattern VK_SAMPLE_COUNT_64_BIT
  , VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , VK_UUID_SIZE
  , pattern VK_UUID_SIZE
  , VK_MAX_MEMORY_TYPES
  , pattern VK_MAX_MEMORY_TYPES
  , VK_MAX_MEMORY_HEAPS
  , pattern VK_MAX_MEMORY_HEAPS
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkVoidFunction
  , VkInstance
  , VkPhysicalDevice
  , VkDevice
  , vkCreateInstance
  , vkDestroyInstance
  , vkEnumeratePhysicalDevices
  , vkGetDeviceProcAddr
  , vkGetInstanceProcAddr
  , vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceQueueFamilyProperties
  , vkGetPhysicalDeviceMemoryProperties
  , vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceFormatProperties
  , vkGetPhysicalDeviceImageFormatProperties
  , VkExtent3D(..)
  , VkPhysicalDeviceProperties(..)
  , VkApplicationInfo(..)
  , VkAllocationCallbacks(..)
  , VkInstanceCreateInfo(..)
  , VkQueueFamilyProperties(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkMemoryType(..)
  , VkMemoryHeap(..)
  , VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkPhysicalDeviceLimits(..)
  , VkQueueFlags
  , VkMemoryPropertyFlags
  , VkMemoryHeapFlags
  , VkImageUsageFlags
  , VkImageCreateFlags
  , VkFormatFeatureFlags
  , VkSampleCountFlags
  , VkDeviceSize
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word64
  , Word8
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )


-- ** VkImageTiling

-- | VkImageTiling - Specifies the tiling arrangement of data in an image
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkImageTiling = VkImageTiling Int32
  deriving (Eq, Ord, Storable)

instance Show VkImageTiling where
  showsPrec _ VK_IMAGE_TILING_OPTIMAL = showString "VK_IMAGE_TILING_OPTIMAL"
  showsPrec _ VK_IMAGE_TILING_LINEAR = showString "VK_IMAGE_TILING_LINEAR"
  showsPrec p (VkImageTiling x) = showParen (p >= 11) (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
  readPrec = parens ( choose [ ("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL)
                             , ("VK_IMAGE_TILING_LINEAR",  pure VK_IMAGE_TILING_LINEAR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageTiling")
                        v <- step readPrec
                        pure (VkImageTiling v)
                        )
                    )

-- | @VK_IMAGE_TILING_OPTIMAL@ specifies optimal tiling (texels are laid out
-- in an implementation-dependent arrangement, for more optimal memory
-- access).
pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling
pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

-- | @VK_IMAGE_TILING_LINEAR@ specifies linear tiling (texels are laid out in
-- memory in row-major order, possibly with some padding on each row).
pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling
pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1
-- ** VkImageType

-- | VkImageType - Specifies the type of an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkImageType = VkImageType Int32
  deriving (Eq, Ord, Storable)

instance Show VkImageType where
  showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
  showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
  showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
  showsPrec p (VkImageType x) = showParen (p >= 11) (showString "VkImageType " . showsPrec 11 x)

instance Read VkImageType where
  readPrec = parens ( choose [ ("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D)
                             , ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D)
                             , ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageType")
                        v <- step readPrec
                        pure (VkImageType v)
                        )
                    )

-- | @VK_IMAGE_TYPE_1D@ specifies a one-dimensional image.
pattern VK_IMAGE_TYPE_1D :: VkImageType
pattern VK_IMAGE_TYPE_1D = VkImageType 0

-- | @VK_IMAGE_TYPE_2D@ specifies a two-dimensional image.
pattern VK_IMAGE_TYPE_2D :: VkImageType
pattern VK_IMAGE_TYPE_2D = VkImageType 1

-- | @VK_IMAGE_TYPE_3D@ specifies a three-dimensional image.
pattern VK_IMAGE_TYPE_3D :: VkImageType
pattern VK_IMAGE_TYPE_3D = VkImageType 2
-- ** VkPhysicalDeviceType

-- | VkPhysicalDeviceType - Supported physical device types
--
-- = Description
--
-- -   @VK_PHYSICAL_DEVICE_TYPE_OTHER@ - the device does not match any
--     other available types.
--
-- -   @VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU@ - the device is typically
--     one embedded in or tightly coupled with the host.
--
-- -   @VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU@ - the device is typically a
--     separate processor connected to the host via an interlink.
--
-- -   @VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU@ - the device is typically a
--     virtual node in a virtualization environment.
--
-- -   @VK_PHYSICAL_DEVICE_TYPE_CPU@ - the device is typically running on
--     the same processors as the host.
--
-- The physical device type is advertised for informational purposes only,
-- and does not directly affect the operation of the system. However, the
-- device type /may/ correlate with other advertised properties or
-- capabilities of the system, such as how many memory heaps there are.
--
-- = See Also
--
-- 'VkPhysicalDeviceProperties'
newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
  deriving (Eq, Ord, Storable)

instance Show VkPhysicalDeviceType where
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_OTHER = showString "VK_PHYSICAL_DEVICE_TYPE_OTHER"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_CPU = showString "VK_PHYSICAL_DEVICE_TYPE_CPU"
  showsPrec p (VkPhysicalDeviceType x) = showParen (p >= 11) (showString "VkPhysicalDeviceType " . showsPrec 11 x)

instance Read VkPhysicalDeviceType where
  readPrec = parens ( choose [ ("VK_PHYSICAL_DEVICE_TYPE_OTHER",          pure VK_PHYSICAL_DEVICE_TYPE_OTHER)
                             , ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU", pure VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU",   pure VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU",    pure VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_CPU",            pure VK_PHYSICAL_DEVICE_TYPE_CPU)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPhysicalDeviceType")
                        v <- step readPrec
                        pure (VkPhysicalDeviceType v)
                        )
                    )

-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_OTHER"
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VkPhysicalDeviceType 1

-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VkPhysicalDeviceType 2

-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VkPhysicalDeviceType 3

-- No documentation found for Nested "VkPhysicalDeviceType" "VK_PHYSICAL_DEVICE_TYPE_CPU"
pattern VK_PHYSICAL_DEVICE_TYPE_CPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4
-- ** VkSystemAllocationScope

-- | VkSystemAllocationScope - Allocation scope
--
-- = Description
--
-- -   @VK_SYSTEM_ALLOCATION_SCOPE_COMMAND@ specifies that the allocation
--     is scoped to the duration of the Vulkan command.
--
-- -   @VK_SYSTEM_ALLOCATION_SCOPE_OBJECT@ specifies that the allocation is
--     scoped to the lifetime of the Vulkan object that is being created or
--     used.
--
-- -   @VK_SYSTEM_ALLOCATION_SCOPE_CACHE@ specifies that the allocation is
--     scoped to the lifetime of a @VkPipelineCache@ or
--     @VkValidationCacheEXT@ object.
--
-- -   @VK_SYSTEM_ALLOCATION_SCOPE_DEVICE@ specifies that the allocation is
--     scoped to the lifetime of the Vulkan device.
--
-- -   @VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE@ specifies that the allocation
--     is scoped to the lifetime of the Vulkan instance.
--
-- Most Vulkan commands operate on a single object, or there is a sole
-- object that is being created or manipulated. When an allocation uses an
-- allocation scope of @VK_SYSTEM_ALLOCATION_SCOPE_OBJECT@ or
-- @VK_SYSTEM_ALLOCATION_SCOPE_CACHE@, the allocation is scoped to the
-- object being created or manipulated.
--
-- When an implementation requires host memory, it will make callbacks to
-- the application using the most specific allocator and allocation scope
-- available:
--
-- -   If an allocation is scoped to the duration of a command, the
--     allocator will use the @VK_SYSTEM_ALLOCATION_SCOPE_COMMAND@
--     allocation scope. The most specific allocator available is used: if
--     the object being created or manipulated has an allocator, that
--     object’s allocator will be used, else if the parent @VkDevice@ has
--     an allocator it will be used, else if the parent @VkInstance@ has an
--     allocator it will be used. Else,
--
-- -   If an allocation is associated with an object of type
--     @VkValidationCacheEXT@ or @VkPipelineCache@, the allocator will use
--     the @VK_SYSTEM_ALLOCATION_SCOPE_CACHE@ allocation scope. The most
--     specific allocator available is used (cache, else device, else
--     instance). Else,
--
-- -   If an allocation is scoped to the lifetime of an object, that object
--     is being created or manipulated by the command, and that object’s
--     type is not @VkDevice@ or @VkInstance@, the allocator will use an
--     allocation scope of @VK_SYSTEM_ALLOCATION_SCOPE_OBJECT@. The most
--     specific allocator available is used (object, else device, else
--     instance). Else,
--
-- -   If an allocation is scoped to the lifetime of a device, the
--     allocator will use an allocation scope of
--     @VK_SYSTEM_ALLOCATION_SCOPE_DEVICE@. The most specific allocator
--     available is used (device, else instance). Else,
--
-- -   If the allocation is scoped to the lifetime of an instance and the
--     instance has an allocator, its allocator will be used with an
--     allocation scope of @VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE@.
--
-- -   Otherwise an implementation will allocate memory through an
--     alternative mechanism that is unspecified.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
  deriving (Eq, Ord, Storable)

instance Show VkSystemAllocationScope where
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = showString "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = showString "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_CACHE = showString "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = showString "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = showString "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
  showsPrec p (VkSystemAllocationScope x) = showParen (p >= 11) (showString "VkSystemAllocationScope " . showsPrec 11 x)

instance Read VkSystemAllocationScope where
  readPrec = parens ( choose [ ("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND",  pure VK_SYSTEM_ALLOCATION_SCOPE_COMMAND)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT",   pure VK_SYSTEM_ALLOCATION_SCOPE_OBJECT)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE",    pure VK_SYSTEM_ALLOCATION_SCOPE_CACHE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE",   pure VK_SYSTEM_ALLOCATION_SCOPE_DEVICE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE", pure VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSystemAllocationScope")
                        v <- step readPrec
                        pure (VkSystemAllocationScope v)
                        )
                    )

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = VkSystemAllocationScope 0

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = VkSystemAllocationScope 1

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = VkSystemAllocationScope 2

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = VkSystemAllocationScope 3

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = VkSystemAllocationScope 4
-- ** VkInternalAllocationType

-- | VkInternalAllocationType - Allocation type
--
-- = See Also
--
-- 'PFN_vkInternalAllocationNotification', 'PFN_vkInternalFreeNotification'
newtype VkInternalAllocationType = VkInternalAllocationType Int32
  deriving (Eq, Ord, Storable)

instance Show VkInternalAllocationType where
  showsPrec _ VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = showString "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
  showsPrec p (VkInternalAllocationType x) = showParen (p >= 11) (showString "VkInternalAllocationType " . showsPrec 11 x)

instance Read VkInternalAllocationType where
  readPrec = parens ( choose [ ("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE", pure VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkInternalAllocationType")
                        v <- step readPrec
                        pure (VkInternalAllocationType v)
                        )
                    )

-- | @VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE@ specifies that the allocation
-- is intended for execution by the host.
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE :: VkInternalAllocationType
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0
-- ** VkInstanceCreateFlags

-- | VkInstanceCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkInstanceCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkInstanceCreateInfo'
newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkInstanceCreateFlags where
  
  showsPrec p (VkInstanceCreateFlags x) = showParen (p >= 11) (showString "VkInstanceCreateFlags " . showsPrec 11 x)

instance Read VkInstanceCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkInstanceCreateFlags")
                        v <- step readPrec
                        pure (VkInstanceCreateFlags v)
                        )
                    )


-- ** VkQueueFlagBits

-- | VkQueueFlagBits - Bitmask specifying capabilities of queues in a queue
-- family
--
-- = Description
--
-- -   @VK_QUEUE_GRAPHICS_BIT@ specifies that queues in this queue family
--     support graphics operations.
--
-- -   @VK_QUEUE_COMPUTE_BIT@ specifies that queues in this queue family
--     support compute operations.
--
-- -   @VK_QUEUE_TRANSFER_BIT@ specifies that queues in this queue family
--     support transfer operations.
--
-- -   @VK_QUEUE_SPARSE_BINDING_BIT@ specifies that queues in this queue
--     family support sparse memory management operations (see [Sparse
--     Resources](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory)).
--     If any of the sparse resource features are enabled, then at least
--     one queue family /must/ support this bit.
--
-- If an implementation exposes any queue family that supports graphics
-- operations, at least one queue family of at least one physical device
-- exposed by the implementation /must/ support both graphics and compute
-- operations.
--
-- __Note__
--
-- All commands that are allowed on a queue that supports transfer
-- operations are also allowed on a queue that supports either graphics or
-- compute operations. Thus, if the capabilities of a queue family include
-- @VK_QUEUE_GRAPHICS_BIT@ or @VK_QUEUE_COMPUTE_BIT@, then reporting the
-- @VK_QUEUE_TRANSFER_BIT@ capability separately for that queue family is
-- /optional/.
--
-- For further details see
-- [Queues](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-queues).
--
-- = See Also
--
-- 'VkQueueFlags'
newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueueFlagBits where
  showsPrec _ VK_QUEUE_GRAPHICS_BIT = showString "VK_QUEUE_GRAPHICS_BIT"
  showsPrec _ VK_QUEUE_COMPUTE_BIT = showString "VK_QUEUE_COMPUTE_BIT"
  showsPrec _ VK_QUEUE_TRANSFER_BIT = showString "VK_QUEUE_TRANSFER_BIT"
  showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT = showString "VK_QUEUE_SPARSE_BINDING_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkQueueFlagBits 0x00000010) = showString "VK_QUEUE_PROTECTED_BIT"
  showsPrec p (VkQueueFlagBits x) = showParen (p >= 11) (showString "VkQueueFlagBits " . showsPrec 11 x)

instance Read VkQueueFlagBits where
  readPrec = parens ( choose [ ("VK_QUEUE_GRAPHICS_BIT",       pure VK_QUEUE_GRAPHICS_BIT)
                             , ("VK_QUEUE_COMPUTE_BIT",        pure VK_QUEUE_COMPUTE_BIT)
                             , ("VK_QUEUE_TRANSFER_BIT",       pure VK_QUEUE_TRANSFER_BIT)
                             , ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_QUEUE_PROTECTED_BIT", pure (VkQueueFlagBits 0x00000010))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueueFlagBits")
                        v <- step readPrec
                        pure (VkQueueFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_GRAPHICS_BIT"
pattern VK_QUEUE_GRAPHICS_BIT :: VkQueueFlagBits
pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 0x00000001

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_COMPUTE_BIT"
pattern VK_QUEUE_COMPUTE_BIT :: VkQueueFlagBits
pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 0x00000002

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_TRANSFER_BIT"
pattern VK_QUEUE_TRANSFER_BIT :: VkQueueFlagBits
pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 0x00000004

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_SPARSE_BINDING_BIT"
pattern VK_QUEUE_SPARSE_BINDING_BIT :: VkQueueFlagBits
pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 0x00000008
-- ** VkMemoryPropertyFlagBits

-- | VkMemoryPropertyFlagBits - Bitmask specifying properties for a memory
-- type
--
-- = See Also
--
-- 'VkMemoryPropertyFlags'
newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMemoryPropertyFlagBits where
  showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkMemoryPropertyFlagBits 0x00000020) = showString "VK_MEMORY_PROPERTY_PROTECTED_BIT"
  showsPrec p (VkMemoryPropertyFlagBits x) = showParen (p >= 11) (showString "VkMemoryPropertyFlagBits " . showsPrec 11 x)

instance Read VkMemoryPropertyFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT",     pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT",     pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT",    pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT",      pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT)
                             , ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT", pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_MEMORY_PROPERTY_PROTECTED_BIT", pure (VkMemoryPropertyFlagBits 0x00000020))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryPropertyFlagBits")
                        v <- step readPrec
                        pure (VkMemoryPropertyFlagBits v)
                        )
                    )

-- | @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ bit specifies that memory
-- allocated with this type is the most efficient for device access. This
-- property will be set if and only if the memory type belongs to a heap
-- with the @VK_MEMORY_HEAP_DEVICE_LOCAL_BIT@ set.
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 0x00000001

-- | @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ bit specifies that memory
-- allocated with this type /can/ be mapped for host access using
-- 'Graphics.Vulkan.Core10.Memory.vkMapMemory'.
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 0x00000002

-- | @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@ bit specifies that the host cache
-- management commands
-- 'Graphics.Vulkan.Core10.Memory.vkFlushMappedMemoryRanges' and
-- 'Graphics.Vulkan.Core10.Memory.vkInvalidateMappedMemoryRanges' are not
-- needed to flush host writes to the device or make device writes visible
-- to the host, respectively.
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 0x00000004

-- | @VK_MEMORY_PROPERTY_HOST_CACHED_BIT@ bit specifies that memory allocated
-- with this type is cached on the host. Host memory accesses to uncached
-- memory are slower than to cached memory, however uncached memory is
-- always host coherent.
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 0x00000008

-- | @VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT@ bit specifies that the memory
-- type only allows device access to the memory. Memory types /must/ not
-- have both @VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT@ and
-- @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ set. Additionally, the object’s
-- backing memory /may/ be provided by the implementation lazily as
-- specified in [Lazily Allocated
-- Memory](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device-lazy_allocation).
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 0x00000010
-- ** VkMemoryHeapFlagBits

-- | VkMemoryHeapFlagBits - Bitmask specifying attribute flags for a heap
--
-- = See Also
--
-- 'VkMemoryHeapFlags'
newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMemoryHeapFlagBits where
  showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkMemoryHeapFlagBits 0x00000002) = showString "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT"
  showsPrec p (VkMemoryHeapFlagBits x) = showParen (p >= 11) (showString "VkMemoryHeapFlagBits " . showsPrec 11 x)

instance Read VkMemoryHeapFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT", pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_MEMORY_HEAP_MULTI_INSTANCE_BIT", pure (VkMemoryHeapFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryHeapFlagBits")
                        v <- step readPrec
                        pure (VkMemoryHeapFlagBits v)
                        )
                    )

-- | @VK_MEMORY_HEAP_DEVICE_LOCAL_BIT@ specifies that the heap corresponds to
-- device local memory. Device local memory /may/ have different
-- performance characteristics than host local memory, and /may/ support
-- different memory property flags.
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 0x00000001
-- ** VkImageUsageFlagBits

-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = See Also
--
-- 'VkImageUsageFlags'
newtype VkImageUsageFlagBits = VkImageUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkImageUsageFlagBits where
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
  showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT = showString "VK_IMAGE_USAGE_STORAGE_BIT"
  showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
  showsPrec p (VkImageUsageFlagBits x) = showParen (p >= 11) (showString "VkImageUsageFlagBits " . showsPrec 11 x)

instance Read VkImageUsageFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_USAGE_TRANSFER_SRC_BIT",             pure VK_IMAGE_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_IMAGE_USAGE_TRANSFER_DST_BIT",             pure VK_IMAGE_USAGE_TRANSFER_DST_BIT)
                             , ("VK_IMAGE_USAGE_SAMPLED_BIT",                  pure VK_IMAGE_USAGE_SAMPLED_BIT)
                             , ("VK_IMAGE_USAGE_STORAGE_BIT",                  pure VK_IMAGE_USAGE_STORAGE_BIT)
                             , ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT",         pure VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT", pure VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT",     pure VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT",         pure VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageUsageFlagBits")
                        v <- step readPrec
                        pure (VkImageUsageFlagBits v)
                        )
                    )

-- | @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ specifies that the image /can/ be used
-- as the source of a transfer command.
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 0x00000001

-- | @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ specifies that the image /can/ be used
-- as the destination of a transfer command.
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 0x00000002

-- | @VK_IMAGE_USAGE_SAMPLED_BIT@ specifies that the image /can/ be used to
-- create a @VkImageView@ suitable for occupying a @VkDescriptorSet@ slot
-- either of type @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ or
-- @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, and be sampled by a shader.
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 0x00000004

-- | @VK_IMAGE_USAGE_STORAGE_BIT@ specifies that the image /can/ be used to
-- create a @VkImageView@ suitable for occupying a @VkDescriptorSet@ slot
-- of type @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@.
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 0x00000008

-- | @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ specifies that the image /can/ be
-- used to create a @VkImageView@ suitable for use as a color or resolve
-- attachment in a @VkFramebuffer@.
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000010

-- | @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ specifies that the image
-- /can/ be used to create a @VkImageView@ suitable for use as a
-- depth\/stencil attachment in a @VkFramebuffer@.
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000020

-- | @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@ specifies that the memory
-- bound to this image will have been allocated with the
-- @VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT@ (see
-- [{html_spec_relative}#memory](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory)
-- for more detail). This bit /can/ be set for any image that /can/ be used
-- to create a @VkImageView@ suitable for use as a color, resolve,
-- depth\/stencil, or input attachment.
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000040

-- | @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ specifies that the image /can/ be
-- used to create a @VkImageView@ suitable for occupying @VkDescriptorSet@
-- slot of type @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@; be read from a
-- shader as an input attachment; and be used as an input attachment in a
-- framebuffer.
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000080
-- ** VkImageCreateFlagBits

-- | VkImageCreateFlagBits - Bitmask specifying additional parameters of an
-- image
--
-- = Description
--
-- -   @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@ specifies that the image will
--     be backed using sparse memory binding.
--
-- -   @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@ specifies that the image
--     /can/ be partially backed using sparse memory binding. Images
--     created with this flag /must/ also be created with the
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@ flag.
--
-- -   @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@ specifies that the image will
--     be backed using sparse memory binding with memory ranges that might
--     also simultaneously be backing another image (or another portion of
--     the same image). Images created with this flag /must/ also be
--     created with the @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@ flag
--
-- -   @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@ specifies that the image /can/
--     be used to create a @VkImageView@ with a different format from the
--     image. For
--     [multi-planar](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     formats, @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@ specifies that a
--     @VkImageView@ can be created of a /plane/ of the image.
--
-- -   @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@ specifies that the image /can/
--     be used to create a @VkImageView@ of type @VK_IMAGE_VIEW_TYPE_CUBE@
--     or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@.
--
-- -   @VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT@ specifies that the image
--     /can/ be used to create a @VkImageView@ of type
--     @VK_IMAGE_VIEW_TYPE_2D@ or @VK_IMAGE_VIEW_TYPE_2D_ARRAY@.
--
-- -   @VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT@ specifies that the
--     image /can/ be used with a non-zero value of the
--     @splitInstanceBindRegionCount@ member of a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'
--     structure passed into
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'.
--     This flag also has the effect of making the image use the standard
--     sparse image block dimensions.
--
-- -   @VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT@ specifies that the
--     image having a compressed format /can/ be used to create a
--     @VkImageView@ with an uncompressed format where each texel in the
--     image view corresponds to a compressed texel block of the image.
--
-- -   @VK_IMAGE_CREATE_EXTENDED_USAGE_BIT@ specifies that the image /can/
--     be created with usage flags that are not supported for the format
--     the image is created with but are supported for at least one format
--     a @VkImageView@ created from the image /can/ have.
--
-- -   @VK_IMAGE_CREATE_DISJOINT_BIT@ specifies that an image with a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     /must/ have each plane separately bound to memory, rather than
--     having a single memory binding for the whole image; the presence of
--     this bit distinguishes a /disjoint image/ from an image without this
--     bit set.
--
-- -   @VK_IMAGE_CREATE_ALIAS_BIT@ specifies that two images created with
--     the same creation parameters and aliased to the same memory /can/
--     interpret the contents of the memory consistently with each other,
--     subject to the rules described in the [Memory
--     Aliasing](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-memory-aliasing)
--     section. This flag further specifies that each plane of a /disjoint/
--     image /can/ share an in-memory non-linear representation with
--     single-plane images, and that a single-plane image /can/ share an
--     in-memory non-linear representation with a plane of a multi-planar
--     disjoint image, according to the rules in
--     [{html_spec_relative}#features-formats-compatible-planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes).
--     If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV'
--     structure whose @handleTypes@ member is not @0@, it is as if
--     @VK_IMAGE_CREATE_ALIAS_BIT@ is set.
--
-- -   @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@
--     specifies that an image with a depth or depth\/stencil format /can/
--     be used with custom sample locations when used as a depth\/stencil
--     attachment.
--
-- See [Sparse Resource
-- Features](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures)
-- and [Sparse Physical Device
-- Features](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-physicalfeatures)
-- for more details.
--
-- = See Also
--
-- 'VkImageCreateFlags'
newtype VkImageCreateFlagBits = VkImageCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkImageCreateFlagBits where
  showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
  showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
  showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageCreateFlagBits 0x00000400) = showString "VK_IMAGE_CREATE_ALIAS_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000040) = showString "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000020) = showString "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000080) = showString "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000100) = showString "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000800) = showString "VK_IMAGE_CREATE_PROTECTED_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000200) = showString "VK_IMAGE_CREATE_DISJOINT_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00001000) = showString "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
  showsPrec p (VkImageCreateFlagBits x) = showParen (p >= 11) (showString "VkImageCreateFlagBits " . showsPrec 11 x)

instance Read VkImageCreateFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_CREATE_SPARSE_BINDING_BIT",   pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT", pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT",   pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT)
                             , ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT",   pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)
                             , ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT",  pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_CREATE_ALIAS_BIT",                                 pure (VkImageCreateFlagBits 0x00000400))
                             , ("VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT",           pure (VkImageCreateFlagBits 0x00000040))
                             , ("VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT",                   pure (VkImageCreateFlagBits 0x00000020))
                             , ("VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT",           pure (VkImageCreateFlagBits 0x00000080))
                             , ("VK_IMAGE_CREATE_EXTENDED_USAGE_BIT",                        pure (VkImageCreateFlagBits 0x00000100))
                             , ("VK_IMAGE_CREATE_PROTECTED_BIT",                             pure (VkImageCreateFlagBits 0x00000800))
                             , ("VK_IMAGE_CREATE_DISJOINT_BIT",                              pure (VkImageCreateFlagBits 0x00000200))
                             , ("VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT", pure (VkImageCreateFlagBits 0x00001000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageCreateFlagBits")
                        v <- step readPrec
                        pure (VkImageCreateFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlagBits 0x00000001

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlagBits 0x00000002

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlagBits 0x00000004

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlagBits 0x00000008

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000010
-- ** VkFormatFeatureFlagBits

-- | VkFormatFeatureFlagBits - Bitmask specifying features supported by a
-- buffer
--
-- = Description
--
-- The following bits /may/ be set in @linearTilingFeatures@ and
-- @optimalTilingFeatures@, specifying that the features are supported by
-- [images](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkImage)
-- or [image
-- views](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkImageView)
-- created with the queried
-- 'vkGetPhysicalDeviceFormatProperties'::@format@:
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@ specifies that an image view
--     /can/ be [sampled
--     from](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampledimage).
--
-- -   @VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT@ specifies that an image view
--     /can/ be used as a [storage
--     images](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storageimage).
--
-- -   @VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT@ specifies that an image
--     view /can/ be used as storage image that supports atomic operations.
--
-- -   @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@ specifies that an image
--     view /can/ be used as a framebuffer color attachment and as an input
--     attachment.
--
-- -   @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT@ specifies that an
--     image view /can/ be used as a framebuffer color attachment that
--     supports blending and as an input attachment.
--
-- -   @VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT@ specifies that an
--     image view /can/ be used as a framebuffer depth\/stencil attachment
--     and as an input attachment.
--
-- -   @VK_FORMAT_FEATURE_BLIT_SRC_BIT@ specifies that an image /can/ be
--     used as @srcImage@ for the @vkCmdBlitImage@ command.
--
-- -   @VK_FORMAT_FEATURE_BLIT_DST_BIT@ specifies that an image /can/ be
--     used as @dstImage@ for the @vkCmdBlitImage@ command.
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ specifies that
--     if @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@ is also set, an image view
--     /can/ be used with a sampler that has either of @magFilter@ or
--     @minFilter@ set to @VK_FILTER_LINEAR@, or @mipmapMode@ set to
--     @VK_SAMPLER_MIPMAP_MODE_LINEAR@. If @VK_FORMAT_FEATURE_BLIT_SRC_BIT@
--     is also set, an image can be used as the @srcImage@ to
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBlitImage' with a
--     @filter@ of @VK_FILTER_LINEAR@. This bit /must/ only be exposed for
--     formats that also support the @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@
--     or @VK_FORMAT_FEATURE_BLIT_SRC_BIT@.
--
--     If the format being queried is a depth\/stencil format, this bit
--     only specifies that the depth aspect (not the stencil aspect) of an
--     image of this format supports linear filtering, and that linear
--     filtering of the depth aspect is supported whether depth compare is
--     enabled in the sampler or not. If this bit is not present, linear
--     filtering with depth compare disabled is unsupported and linear
--     filtering with depth compare enabled is supported, but /may/ compute
--     the filtered value in an implementation-dependent manner which
--     differs from the normal rules of linear filtering. The resulting
--     value /must/ be in the range [0,1] and /should/ be proportional to,
--     or a weighted average of, the number of comparison passes or
--     failures.
--
-- -   @VK_FORMAT_FEATURE_TRANSFER_SRC_BIT@ specifies that an image /can/
--     be used as a source image for [copy
--     commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies).
--
-- -   @VK_FORMAT_FEATURE_TRANSFER_DST_BIT@ specifies that an image /can/
--     be used as a destination image for [copy
--     commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies)
--     and [clear
--     commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears).
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT@ specifies
--     @VkImage@ /can/ be used as a sampled image with a min or max
--     'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeEXT'.
--     This bit /must/ only be exposed for formats that also support the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@.
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ specifies
--     that @VkImage@ /can/ be used with a sampler that has either of
--     @magFilter@ or @minFilter@ set to @VK_FILTER_CUBIC_IMG@, or be the
--     source image for a blit with @filter@ set to @VK_FILTER_CUBIC_IMG@.
--     This bit /must/ only be exposed for formats that also support the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@. If the format being queried
--     is a depth\/stencil format, this only specifies that the depth
--     aspect is cubic filterable.
--
-- -   @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@ specifies that an
--     application /can/ define a [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     using this format as a source, and that an image of this format
--     /can/ be used with a @VkSamplerYcbcrConversionCreateInfo@
--     @xChromaOffset@ and\/or @yChromaOffset@ of
--     @VK_CHROMA_LOCATION_MIDPOINT@. Otherwise both @xChromaOffset@ and
--     @yChromaOffset@ /must/ be @VK_CHROMA_LOCATION_COSITED_EVEN@. If a
--     format does not incorporate chroma downsampling (it is not a “422”
--     or “420” format) but the implementation supports sampler Y’CBCR
--     conversion for this format, the implementation /must/ set
--     @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@.
--
-- -   @VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT@ specifies that an
--     application /can/ define a [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     using this format as a source, and that an image of this format
--     /can/ be used with a @VkSamplerYcbcrConversionCreateInfo@
--     @xChromaOffset@ and\/or @yChromaOffset@ of
--     @VK_CHROMA_LOCATION_COSITED_EVEN@. Otherwise both @xChromaOffset@
--     and @yChromaOffset@ /must/ be @VK_CHROMA_LOCATION_MIDPOINT@. If
--     neither @VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT@ nor
--     @VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT@ is set, the
--     application /must/ not define a [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     using this format as a source.
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT@
--     specifies that the format can do linear sampler filtering
--     (min\/magFilter) whilst sampler Y’CBCR conversion is enabled.
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT@
--     specifies that the format can have different chroma, min, and mag
--     filters.
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT@
--     specifies that reconstruction is explicit, as described in
--     [{html_spec_relative}#textures-chroma-reconstruction](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-chroma-reconstruction).
--     If this bit is not present, reconstruction is implicit by default.
--
-- -   @VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT@
--     specifies that reconstruction /can/ be forcibly made explicit by
--     setting
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'::@forceExplicitReconstruction@
--     to @VK_TRUE@.
--
-- -   @VK_FORMAT_FEATURE_DISJOINT_BIT@ specifies that a multi-planar image
--     /can/ have the @VK_IMAGE_CREATE_DISJOINT_BIT@ set during image
--     creation. An implementation /must/ not set
--     @VK_FORMAT_FEATURE_DISJOINT_BIT@ for /single-plane formats/.
--
-- The following bits /may/ be set in @bufferFeatures@, specifying that the
-- features are supported by
-- [buffers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkBuffer)
-- or [buffer
-- views](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkBufferView)
-- created with the queried 'vkGetPhysicalDeviceProperties'::@format@:
--
-- -   @VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT@ specifies that the
--     format /can/ be used to create a buffer view that /can/ be bound to
--     a @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ descriptor.
--
-- -   @VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT@ specifies that the
--     format /can/ be used to create a buffer view that /can/ be bound to
--     a @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ descriptor.
--
-- -   @VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT@ specifies that
--     atomic operations are supported on
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ with this format.
--
-- -   @VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT@ specifies that the format
--     /can/ be used as a vertex attribute format
--     (@VkVertexInputAttributeDescription@::@format@).
--
-- = See Also
--
-- 'VkFormatFeatureFlags'
newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkFormatFeatureFlagBits where
  showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = showString "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
  showsPrec _ VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_FORMAT_FEATURE_BLIT_SRC_BIT = showString "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_BLIT_DST_BIT = showString "VK_FORMAT_FEATURE_BLIT_DST_BIT"
  showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkFormatFeatureFlagBits 0x00004000) = showString "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00008000) = showString "VK_FORMAT_FEATURE_TRANSFER_DST_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00020000) = showString "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00040000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00080000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00100000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00200000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00400000) = showString "VK_FORMAT_FEATURE_DISJOINT_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00800000) = showString "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00002000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG"
  showsPrec _ (VkFormatFeatureFlagBits 0x00010000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
  showsPrec p (VkFormatFeatureFlagBits x) = showParen (p >= 11) (showString "VkFormatFeatureFlagBits " . showsPrec 11 x)

instance Read VkFormatFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT",               pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT",               pure VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT",        pure VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT)
                             , ("VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT",        pure VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT",        pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT", pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT)
                             , ("VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT",               pure VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT",            pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT)
                             , ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT",      pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT)
                             , ("VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT",    pure VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)
                             , ("VK_FORMAT_FEATURE_BLIT_SRC_BIT",                    pure VK_FORMAT_FEATURE_BLIT_SRC_BIT)
                             , ("VK_FORMAT_FEATURE_BLIT_DST_BIT",                    pure VK_FORMAT_FEATURE_BLIT_DST_BIT)
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT", pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_FORMAT_FEATURE_TRANSFER_SRC_BIT",                                                            pure (VkFormatFeatureFlagBits 0x00004000))
                             , ("VK_FORMAT_FEATURE_TRANSFER_DST_BIT",                                                            pure (VkFormatFeatureFlagBits 0x00008000))
                             , ("VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT",                                                 pure (VkFormatFeatureFlagBits 0x00020000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT",                            pure (VkFormatFeatureFlagBits 0x00040000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT",           pure (VkFormatFeatureFlagBits 0x00080000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT",           pure (VkFormatFeatureFlagBits 0x00100000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT", pure (VkFormatFeatureFlagBits 0x00200000))
                             , ("VK_FORMAT_FEATURE_DISJOINT_BIT",                                                                pure (VkFormatFeatureFlagBits 0x00400000))
                             , ("VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT",                                                  pure (VkFormatFeatureFlagBits 0x00800000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG",                                          pure (VkFormatFeatureFlagBits 0x00002000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT",                                         pure (VkFormatFeatureFlagBits 0x00010000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFormatFeatureFlagBits")
                        v <- step readPrec
                        pure (VkFormatFeatureFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VkFormatFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = VkFormatFeatureFlagBits 0x00000002

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VkFormatFeatureFlagBits 0x00000004

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000008

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000010

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VkFormatFeatureFlagBits 0x00000020

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000040

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x00000080

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VkFormatFeatureFlagBits 0x00000100

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x00000200

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = VkFormatFeatureFlagBits 0x00000400

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_BLIT_DST_BIT"
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = VkFormatFeatureFlagBits 0x00000800

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VkFormatFeatureFlagBits 0x00001000
-- ** VkSampleCountFlagBits

-- | VkSampleCountFlagBits - Bitmask specifying sample counts supported for
-- an image used for storage operations
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'VkSampleCountFlags',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSampleCountFlagBits where
  showsPrec _ VK_SAMPLE_COUNT_1_BIT = showString "VK_SAMPLE_COUNT_1_BIT"
  showsPrec _ VK_SAMPLE_COUNT_2_BIT = showString "VK_SAMPLE_COUNT_2_BIT"
  showsPrec _ VK_SAMPLE_COUNT_4_BIT = showString "VK_SAMPLE_COUNT_4_BIT"
  showsPrec _ VK_SAMPLE_COUNT_8_BIT = showString "VK_SAMPLE_COUNT_8_BIT"
  showsPrec _ VK_SAMPLE_COUNT_16_BIT = showString "VK_SAMPLE_COUNT_16_BIT"
  showsPrec _ VK_SAMPLE_COUNT_32_BIT = showString "VK_SAMPLE_COUNT_32_BIT"
  showsPrec _ VK_SAMPLE_COUNT_64_BIT = showString "VK_SAMPLE_COUNT_64_BIT"
  showsPrec p (VkSampleCountFlagBits x) = showParen (p >= 11) (showString "VkSampleCountFlagBits " . showsPrec 11 x)

instance Read VkSampleCountFlagBits where
  readPrec = parens ( choose [ ("VK_SAMPLE_COUNT_1_BIT",  pure VK_SAMPLE_COUNT_1_BIT)
                             , ("VK_SAMPLE_COUNT_2_BIT",  pure VK_SAMPLE_COUNT_2_BIT)
                             , ("VK_SAMPLE_COUNT_4_BIT",  pure VK_SAMPLE_COUNT_4_BIT)
                             , ("VK_SAMPLE_COUNT_8_BIT",  pure VK_SAMPLE_COUNT_8_BIT)
                             , ("VK_SAMPLE_COUNT_16_BIT", pure VK_SAMPLE_COUNT_16_BIT)
                             , ("VK_SAMPLE_COUNT_32_BIT", pure VK_SAMPLE_COUNT_32_BIT)
                             , ("VK_SAMPLE_COUNT_64_BIT", pure VK_SAMPLE_COUNT_64_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSampleCountFlagBits")
                        v <- step readPrec
                        pure (VkSampleCountFlagBits v)
                        )
                    )

-- | @VK_SAMPLE_COUNT_1_BIT@ specifies an image with one sample per pixel.
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x00000001

-- | @VK_SAMPLE_COUNT_2_BIT@ specifies an image with 2 samples per pixel.
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x00000002

-- | @VK_SAMPLE_COUNT_4_BIT@ specifies an image with 4 samples per pixel.
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x00000004

-- | @VK_SAMPLE_COUNT_8_BIT@ specifies an image with 8 samples per pixel.
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x00000008

-- | @VK_SAMPLE_COUNT_16_BIT@ specifies an image with 16 samples per pixel.
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x00000010

-- | @VK_SAMPLE_COUNT_32_BIT@ specifies an image with 32 samples per pixel.
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x00000020

-- | @VK_SAMPLE_COUNT_64_BIT@ specifies an image with 64 samples per pixel.
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x00000040
-- No documentation found for TopLevel "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE"
type VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE"
pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE :: Integral a => a
pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
-- No documentation found for TopLevel "VK_UUID_SIZE"
type VK_UUID_SIZE = 16
-- No documentation found for Nested "Integral a => a" "VK_UUID_SIZE"
pattern VK_UUID_SIZE :: Integral a => a
pattern VK_UUID_SIZE = 16
-- No documentation found for TopLevel "VK_MAX_MEMORY_TYPES"
type VK_MAX_MEMORY_TYPES = 32
-- No documentation found for Nested "Integral a => a" "VK_MAX_MEMORY_TYPES"
pattern VK_MAX_MEMORY_TYPES :: Integral a => a
pattern VK_MAX_MEMORY_TYPES = 32
-- No documentation found for TopLevel "VK_MAX_MEMORY_HEAPS"
type VK_MAX_MEMORY_HEAPS = 16
-- No documentation found for Nested "Integral a => a" "VK_MAX_MEMORY_HEAPS"
pattern VK_MAX_MEMORY_HEAPS :: Integral a => a
pattern VK_MAX_MEMORY_HEAPS = 16
-- | PFN_vkInternalAllocationNotification - Application-defined memory
-- allocation notification function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @size@ is the requested size of an allocation.
--
-- -   @allocationType@ is a 'VkInternalAllocationType' value specifying
--     the requested type of an allocation.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-host-allocation-scope).
--
-- = Description
--
-- This is a purely informational callback.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkInternalAllocationNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())
-- | PFN_vkInternalFreeNotification - Application-defined memory free
-- notification function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @size@ is the requested size of an allocation.
--
-- -   @allocationType@ is a 'VkInternalAllocationType' value specifying
--     the requested type of an allocation.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-host-allocation-scope).
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkInternalFreeNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())
-- | PFN_vkReallocationFunction - Application-defined memory reallocation
-- function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @pOriginal@ /must/ be either @NULL@ or a pointer previously returned
--     by @pfnReallocation@ or @pfnAllocation@ of the same allocator.
--
-- -   @size@ is the size in bytes of the requested allocation.
--
-- -   @alignment@ is the requested alignment of the allocation in bytes
--     and /must/ be a power of two.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-host-allocation-scope).
--
-- = Description
--
-- @pfnReallocation@ /must/ return an allocation with enough space for
-- @size@ bytes, and the contents of the original allocation from bytes
-- zero to min(original size, new size) - 1 /must/ be preserved in the
-- returned allocation. If @size@ is larger than the old size, the contents
-- of the additional space are undefined. If satisfying these requirements
-- involves creating a new allocation, then the old allocation /should/ be
-- freed.
--
-- If @pOriginal@ is @NULL@, then @pfnReallocation@ /must/ behave
-- equivalently to a call to 'PFN_vkAllocationFunction' with the same
-- parameter values (without @pOriginal@).
--
-- If @size@ is zero, then @pfnReallocation@ /must/ behave equivalently to
-- a call to 'PFN_vkFreeFunction' with the same @pUserData@ parameter
-- value, and @pMemory@ equal to @pOriginal@.
--
-- If @pOriginal@ is non-@NULL@, the implementation /must/ ensure that
-- @alignment@ is equal to the @alignment@ used to originally allocate
-- @pOriginal@.
--
-- If this function fails and @pOriginal@ is non-@NULL@ the application
-- /must/ not free the old allocation.
--
-- @pfnReallocation@ /must/ follow the same [rules for return values as
-- @PFN_vkAllocationFunction@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkAllocationFunction_return_rules).
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkReallocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))
-- | PFN_vkAllocationFunction - Application-defined memory allocation
-- function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @size@ is the size in bytes of the requested allocation.
--
-- -   @alignment@ is the requested alignment of the allocation in bytes
--     and /must/ be a power of two.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-host-allocation-scope).
--
-- = Description
--
-- If @pfnAllocation@ is unable to allocate the requested memory, it /must/
-- return @NULL@. If the allocation was successful, it /must/ return a
-- valid pointer to memory allocation containing at least @size@ bytes, and
-- with the pointer value being a multiple of @alignment@.
--
-- __Note__
--
-- Correct Vulkan operation /cannot/ be assumed if the application does not
-- follow these rules.
--
-- For example, @pfnAllocation@ (or @pfnReallocation@) could cause
-- termination of running Vulkan instance(s) on a failed allocation for
-- debugging purposes, either directly or indirectly. In these
-- circumstances, it /cannot/ be assumed that any part of any affected
-- 'VkInstance' objects are going to operate correctly (even
-- 'vkDestroyInstance'), and the application /must/ ensure it cleans up
-- properly via other means (e.g. process termination).
--
-- If @pfnAllocation@ returns @NULL@, and if the implementation is unable
-- to continue correct processing of the current command without the
-- requested allocation, it /must/ treat this as a run-time error, and
-- generate @VK_ERROR_OUT_OF_HOST_MEMORY@ at the appropriate time for the
-- command in which the condition was detected, as described in [Return
-- Codes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes).
--
-- If the implementation is able to continue correct processing of the
-- current command without the requested allocation, then it /may/ do so,
-- and /must/ not generate @VK_ERROR_OUT_OF_HOST_MEMORY@ as a result of
-- this failed allocation.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkAllocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))
-- | PFN_vkFreeFunction - Application-defined memory free function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @pMemory@ is the allocation to be freed.
--
-- = Description
--
-- @pMemory@ /may/ be @NULL@, which the callback /must/ handle safely. If
-- @pMemory@ is non-@NULL@, it /must/ be a pointer previously allocated by
-- @pfnAllocation@ or @pfnReallocation@. The application /should/ free this
-- memory.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkFreeFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ())
-- | PFN_vkVoidFunction - Dummy function pointer type returned by queries
--
-- = See Also
--
-- 'vkGetDeviceProcAddr', 'vkGetInstanceProcAddr'
type PFN_vkVoidFunction = Ptr (() -> IO ())
-- | Dummy data to tag the 'Ptr' with
data VkInstance_T
-- | VkInstance - Opaque handle to a instance object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.vkCreateDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkCreateDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkCreateDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK',
-- 'vkCreateInstance',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_KHR_mir_surface.vkCreateMirSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.vkCreateViSurfaceNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.vkDebugReportMessageEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.vkDestroyDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT',
-- 'vkDestroyInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkDestroySurfaceKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroups',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroupsKHR',
-- 'vkEnumeratePhysicalDevices', 'vkGetInstanceProcAddr',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkSubmitDebugUtilsMessageEXT'
type VkInstance = Ptr VkInstance_T
-- | Dummy data to tag the 'Ptr' with
data VkPhysicalDevice_T
-- | VkPhysicalDevice - Opaque handle to a physical device object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.vkAcquireXlibDisplayEXT',
-- 'Graphics.Vulkan.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.Core10.ExtensionDiscovery.vkEnumerateDeviceExtensionProperties',
-- 'Graphics.Vulkan.Core10.LayerDiscovery.vkEnumerateDeviceLayerProperties',
-- 'vkEnumeratePhysicalDevices',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetDisplayPlaneSupportedDisplaysKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferPropertiesKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFencePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphorePropertiesKHR',
-- 'vkGetPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2KHR',
-- 'vkGetPhysicalDeviceFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2KHR',
-- 'vkGetPhysicalDeviceMemoryProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_mir_surface.vkGetPhysicalDeviceMirPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetPhysicalDevicePresentRectanglesKHR',
-- 'vkGetPhysicalDeviceProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2KHR',
-- 'vkGetPhysicalDeviceQueueFamilyProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2KHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceFormats2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceFormatsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.vkGetPhysicalDeviceWaylandPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.vkGetPhysicalDeviceWin32PresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.vkGetPhysicalDeviceXcbPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.vkGetPhysicalDeviceXlibPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.vkGetRandROutputDisplayEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display.vkReleaseDisplayEXT'
type VkPhysicalDevice = Ptr VkPhysicalDevice_T
-- | Dummy data to tag the 'Ptr' with
data VkDevice_T
-- | VkDevice - Opaque handle to a device object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkAcquireNextImage2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_bind_memory2.vkBindBufferMemory2KHR',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_bind_memory2.vkBindImageMemory2KHR',
-- 'Graphics.Vulkan.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.Core10.Event.vkCreateEvent',
-- 'Graphics.Vulkan.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.Core10.Pass.vkCreateFramebuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCreateIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCreateObjectTableNVX',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.vkCreateQueryPool',
-- 'Graphics.Vulkan.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.Core10.Sampler.vkCreateSampler',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.Core10.Shader.vkCreateShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.vkCreateSharedSwapchainsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.vkDebugMarkerSetObjectNameEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.vkDebugMarkerSetObjectTagEXT',
-- 'Graphics.Vulkan.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.vkDestroyBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkDestroyDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkDestroyDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.vkDestroyDevice',
-- 'Graphics.Vulkan.Core10.Event.vkDestroyEvent',
-- 'Graphics.Vulkan.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.Core10.Pass.vkDestroyFramebuffer',
-- 'Graphics.Vulkan.Core10.Image.vkDestroyImage',
-- 'Graphics.Vulkan.Core10.ImageView.vkDestroyImageView',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkDestroyIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkDestroyObjectTableNVX',
-- 'Graphics.Vulkan.Core10.Pipeline.vkDestroyPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkDestroyPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.vkDestroyPipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.vkDestroyQueryPool',
-- 'Graphics.Vulkan.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.Core10.Sampler.vkDestroySampler',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.vkDestroySemaphore',
-- 'Graphics.Vulkan.Core10.Shader.vkDestroyShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkDestroySwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.vkDestroyValidationCacheEXT',
-- 'Graphics.Vulkan.Core10.Queue.vkDeviceWaitIdle',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkDisplayPowerControlEXT',
-- 'Graphics.Vulkan.Core10.Memory.vkFlushMappedMemoryRanges',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkGetBufferMemoryRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupportKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeatures',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeaturesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetDeviceGroupPresentCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'vkGetDeviceProcAddr', 'Graphics.Vulkan.Core10.Queue.vkGetDeviceQueue',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.Core10.Event.vkGetEventStatus',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.vkGetFenceFdKHR',
-- 'Graphics.Vulkan.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.vkGetFenceWin32HandleKHR',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkGetImageMemoryRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetMemoryAndroidHardwareBufferANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.vkGetPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkGetPipelineCacheData',
-- 'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.vkGetRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.Core10.Pass.vkGetRenderAreaGranularity',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.vkGetSemaphoreFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.vkGetSemaphoreWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkGetSwapchainCounterEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetSwapchainImagesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.vkGetSwapchainStatusKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.vkImportFenceFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.vkImportFenceWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.vkImportSemaphoreFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.vkImportSemaphoreWin32HandleKHR',
-- 'Graphics.Vulkan.Core10.Memory.vkInvalidateMappedMemoryRanges',
-- 'Graphics.Vulkan.Core10.Memory.vkMapMemory',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkMergePipelineCaches',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.vkMergeValidationCachesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDeviceEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkRegisterObjectsNVX',
-- 'Graphics.Vulkan.Core10.CommandPool.vkResetCommandPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkResetDescriptorPool',
-- 'Graphics.Vulkan.Core10.Event.vkResetEvent',
-- 'Graphics.Vulkan.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkSetDebugUtilsObjectNameEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkSetDebugUtilsObjectTagEXT',
-- 'Graphics.Vulkan.Core10.Event.vkSetEvent',
-- 'Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata.vkSetHdrMetadataEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance1.vkTrimCommandPoolKHR',
-- 'Graphics.Vulkan.Core10.Memory.vkUnmapMemory',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkUnregisterObjectsNVX',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkUpdateDescriptorSets',
-- 'Graphics.Vulkan.Core10.Fence.vkWaitForFences'
type VkDevice = Ptr VkDevice_T
-- | vkCreateInstance - Create a new Vulkan instance
--
-- = Parameters
--
-- -   @pCreateInfo@ points to an instance of 'VkInstanceCreateInfo'
--     controlling creation of the instance.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pInstance@ points a @VkInstance@ handle in which the resulting
--     instance is returned.
--
-- = Description
--
-- @vkCreateInstance@ verifies that the requested layers exist. If not,
-- @vkCreateInstance@ will return @VK_ERROR_LAYER_NOT_PRESENT@. Next
-- @vkCreateInstance@ verifies that the requested extensions are supported
-- (e.g. in the implementation or in any enabled instance layer) and if any
-- requested extension is not supported, @vkCreateInstance@ /must/ return
-- @VK_ERROR_EXTENSION_NOT_PRESENT@. After verifying and enabling the
-- instance layers and extensions the @VkInstance@ object is created and
-- returned to the application. If a requested extension is only supported
-- by a layer, both the layer and the extension need to be specified at
-- @vkCreateInstance@ time for the creation to succeed.
--
-- == Valid Usage
--
-- -   All [required
--     extensions](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#extended-functionality-extensions-dependencies)
--     for each extension in the
--     'VkInstanceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also
--     be present in that list.
--
-- == Valid Usage (Implicit)
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkInstanceCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pInstance@ /must/ be a valid pointer to a @VkInstance@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INITIALIZATION_FAILED@
--
--     -   @VK_ERROR_LAYER_NOT_PRESENT@
--
--     -   @VK_ERROR_EXTENSION_NOT_PRESENT@
--
--     -   @VK_ERROR_INCOMPATIBLE_DRIVER@
--
-- = See Also
--
-- 'VkAllocationCallbacks', 'VkInstance', 'VkInstanceCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateInstance" vkCreateInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
-- | vkDestroyInstance - Destroy an instance of Vulkan
--
-- = Parameters
--
-- -   @instance@ is the handle of the instance to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All child objects created using @instance@ /must/ have been
--     destroyed prior to destroying @instance@
--
-- -   If @VkAllocationCallbacks@ were provided when @instance@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @instance@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   If @instance@ is not @NULL@, @instance@ /must/ be a valid
--     @VkInstance@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- == Host Synchronization
--
-- -   Host access to @instance@ /must/ be externally synchronized
--
-- = See Also
--
-- 'VkAllocationCallbacks', 'VkInstance'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyInstance" vkDestroyInstance :: ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkEnumeratePhysicalDevices - Enumerates the physical devices accessible
-- to a Vulkan instance
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'vkCreateInstance'.
--
-- -   @pPhysicalDeviceCount@ is a pointer to an integer related to the
--     number of physical devices available or queried, as described below.
--
-- -   @pPhysicalDevices@ is either @NULL@ or a pointer to an array of
--     @VkPhysicalDevice@ handles.
--
-- = Description
--
-- If @pPhysicalDevices@ is @NULL@, then the number of physical devices
-- available is returned in @pPhysicalDeviceCount@. Otherwise,
-- @pPhysicalDeviceCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pPhysicalDevices@ array, and on return the
-- variable is overwritten with the number of handles actually written to
-- @pPhysicalDevices@. If @pPhysicalDeviceCount@ is less than the number of
-- physical devices available, at most @pPhysicalDeviceCount@ structures
-- will be written. If @pPhysicalDeviceCount@ is smaller than the number of
-- physical devices available, @VK_INCOMPLETE@ will be returned instead of
-- @VK_SUCCESS@, to indicate that not all the available physical devices
-- were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pPhysicalDeviceCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPhysicalDeviceCount@ is not @0@, and
--     @pPhysicalDevices@ is not @NULL@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @pPhysicalDeviceCount@
--     @VkPhysicalDevice@ handles
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INITIALIZATION_FAILED@
--
-- = See Also
--
-- 'VkInstance', 'VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
-- | vkGetDeviceProcAddr - Return a function pointer for a command
--
-- = Parameters
--
-- The table below defines the various use cases for @vkGetDeviceProcAddr@
-- and expected return value for each case.
--
-- = Description
--
-- The returned function pointer is of type 'PFN_vkVoidFunction', and must
-- be cast to the type of the command being queried. The function pointer
-- /must/ only be called with a dispatchable object (the first parameter)
-- that is @device@ or a child of @device@.
--
-- +-----------------------+-----------------------+-----------------------+
-- | @device@              | @pName@               | return value          |
-- +=======================+=======================+=======================+
-- | @NULL@                | *                     | undefined             |
-- +-----------------------+-----------------------+-----------------------+
-- | invalid device        | *                     | undefined             |
-- +-----------------------+-----------------------+-----------------------+
-- | device                | @NULL@                | undefined             |
-- +-----------------------+-----------------------+-----------------------+
-- | device                | core device-level     | fp                    |
-- |                       | Vulkan command        |                       |
-- +-----------------------+-----------------------+-----------------------+
-- | device                | enabled device        | fp                    |
-- |                       | extension commands    |                       |
-- +-----------------------+-----------------------+-----------------------+
-- | device                | * (any @pName@ not    | @NULL@                |
-- |                       | covered above)        |                       |
-- +-----------------------+-----------------------+-----------------------+
--
-- vkGetDeviceProcAddr behavior
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'PFN_vkVoidFunction', 'VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceProcAddr" vkGetDeviceProcAddr :: ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- | vkGetInstanceProcAddr - Return a function pointer for a command
--
-- = Parameters
--
-- -   @instance@ is the instance that the function pointer will be
--     compatible with, or @NULL@ for commands not dependent on any
--     instance.
--
-- -   @pName@ is the name of the command to obtain.
--
-- = Description
--
-- @vkGetInstanceProcAddr@ itself is obtained in a platform- and loader-
-- specific manner. Typically, the loader library will export this command
-- as a function symbol, so applications /can/ link against the loader
-- library, or load it dynamically and look up the symbol using
-- platform-specific APIs.
--
-- The table below defines the various use cases for
-- @vkGetInstanceProcAddr@ and expected return value (“fp” is “function
-- pointer”) for each case.
--
-- The returned function pointer is of type 'PFN_vkVoidFunction', and must
-- be cast to the type of the command being queried.
--
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | @instance@            | @pName@                                                                            | return value          |
-- +=======================+====================================================================================+=======================+
-- | *                     | @NULL@                                                                             | undefined             |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | invalid instance      | *                                                                                  | undefined             |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | @NULL@                | 'Graphics.Vulkan.Core10.ExtensionDiscovery.vkEnumerateInstanceExtensionProperties' | fp                    |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | @NULL@                | 'Graphics.Vulkan.Core10.LayerDiscovery.vkEnumerateInstanceLayerProperties'         | fp                    |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | @NULL@                | 'vkCreateInstance'                                                                 | fp                    |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | @NULL@                | * (any @pName@ not covered above)                                                  | @NULL@                |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | instance              | core Vulkan command                                                                | fp1                   |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | instance              | enabled instance extension commands for @instance@                                 | fp1                   |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | instance              | available device extension2 commands for @instance@                                | fp1                   |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
-- | instance              | * (any @pName@ not covered above)                                                  | @NULL@                |
-- +-----------------------+------------------------------------------------------------------------------------+-----------------------+
--
-- vkGetInstanceProcAddr behavior
--
-- [1]
--     The returned function pointer /must/ only be called with a
--     dispatchable object (the first parameter) that is @instance@ or a
--     child of @instance@, e.g. 'VkInstance', 'VkPhysicalDevice',
--     'VkDevice', 'Graphics.Vulkan.Core10.Queue.VkQueue', or
--     'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'.
--
-- [2]
--     An “available device extension” is a device extension supported by
--     any physical device enumerated by @instance@.
--
-- == Valid Usage (Implicit)
--
-- -   If @instance@ is not @NULL@, @instance@ /must/ be a valid
--     @VkInstance@ handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'PFN_vkVoidFunction', 'VkInstance'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" vkGetInstanceProcAddr :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- | vkGetPhysicalDeviceProperties - Returns properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pProperties@ points to an instance of the
--     'VkPhysicalDeviceProperties' structure, that will be filled with
--     returned information.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pProperties@ /must/ be a valid pointer to a
--     @VkPhysicalDeviceProperties@ structure
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkPhysicalDeviceProperties'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceProperties" vkGetPhysicalDeviceProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
-- | vkGetPhysicalDeviceQueueFamilyProperties - Reports properties of the
-- queues of the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pQueueFamilyPropertyCount@ is a pointer to an integer related to
--     the number of queue families available or queried, as described
--     below.
--
-- -   @pQueueFamilyProperties@ is either @NULL@ or a pointer to an array
--     of 'VkQueueFamilyProperties' structures.
--
-- = Description
--
-- If @pQueueFamilyProperties@ is @NULL@, then the number of queue families
-- available is returned in @pQueueFamilyPropertyCount@. Otherwise,
-- @pQueueFamilyPropertyCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pQueueFamilyProperties@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pQueueFamilyProperties@. If
-- @pQueueFamilyPropertyCount@ is less than the number of queue families
-- available, at most @pQueueFamilyPropertyCount@ structures will be
-- written.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     @VkQueueFamilyProperties@ structures
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkQueueFamilyProperties'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
-- | vkGetPhysicalDeviceMemoryProperties - Reports memory information for the
-- specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the device to query.
--
-- -   @pMemoryProperties@ points to an instance of
--     @VkPhysicalDeviceMemoryProperties@ structure in which the properties
--     are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pMemoryProperties@ /must/ be a valid pointer to a
--     @VkPhysicalDeviceMemoryProperties@ structure
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkPhysicalDeviceMemoryProperties'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
-- | vkGetPhysicalDeviceFeatures - Reports capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     supported features.
--
-- -   @pFeatures@ is a pointer to a 'VkPhysicalDeviceFeatures' structure
--     in which the physical device features are returned. For each
--     feature, a value of @VK_TRUE@ specifies that the feature is
--     supported on this physical device, and @VK_FALSE@ specifies that the
--     feature is not supported.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @pFeatures@ /must/ be a valid pointer to a
--     @VkPhysicalDeviceFeatures@ structure
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkPhysicalDeviceFeatures'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
-- | vkGetPhysicalDeviceFormatProperties - Lists physical device’s format
-- capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     format properties.
--
-- -   @format@ is the format whose properties are queried.
--
-- -   @pFormatProperties@ is a pointer to a 'VkFormatProperties' structure
--     in which physical device properties for @format@ are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- -   @pFormatProperties@ /must/ be a valid pointer to a
--     @VkFormatProperties@ structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkFormat', 'VkFormatProperties',
-- 'VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
-- | vkGetPhysicalDeviceImageFormatProperties - Lists physical device’s image
-- format capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities.
--
-- -   @format@ is a 'Graphics.Vulkan.Core10.Core.VkFormat' value
--     specifying the image format, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@format@.
--
-- -   @type@ is a 'VkImageType' value specifying the image type,
--     corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@imageType@.
--
-- -   @tiling@ is a 'VkImageTiling' value specifying the image tiling,
--     corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@tiling@.
--
-- -   @usage@ is a bitmask of 'VkImageUsageFlagBits' specifying the
--     intended usage of the image, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@usage@.
--
-- -   @flags@ is a bitmask of 'VkImageCreateFlagBits' specifying
--     additional parameters of the image, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@flags@.
--
-- -   @pImageFormatProperties@ points to an instance of the
--     'VkImageFormatProperties' structure in which capabilities are
--     returned.
--
-- = Description
--
-- The @format@, @type@, @tiling@, @usage@, and @flags@ parameters
-- correspond to parameters that would be consumed by
-- 'Graphics.Vulkan.Core10.Image.vkCreateImage' (as members of
-- @VkImageCreateInfo@).
--
-- If @format@ is not a supported image format, or if the combination of
-- @format@, @type@, @tiling@, @usage@, and @flags@ is not supported for
-- images, then @vkGetPhysicalDeviceImageFormatProperties@ returns
-- @VK_ERROR_FORMAT_NOT_SUPPORTED@.
--
-- The limitations on an image format that are reported by
-- @vkGetPhysicalDeviceImageFormatProperties@ have the following property:
-- if @usage1@ and @usage2@ of type 'VkImageUsageFlags' are such that the
-- bits set in @usage1@ are a subset of the bits set in @usage2@, and
-- @flags1@ and @flags2@ of type 'VkImageCreateFlags' are such that the
-- bits set in @flags1@ are a subset of the bits set in @flags2@, then the
-- limitations for @usage1@ and @flags1@ /must/ be no more strict than the
-- limitations for @usage2@ and @flags2@, for all values of @format@,
-- @type@, and @tiling@.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- -   @type@ /must/ be a valid 'VkImageType' value
--
-- -   @tiling@ /must/ be a valid 'VkImageTiling' value
--
-- -   @usage@ /must/ be a valid combination of 'VkImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @flags@ /must/ be a valid combination of 'VkImageCreateFlagBits'
--     values
--
-- -   @pImageFormatProperties@ /must/ be a valid pointer to a
--     @VkImageFormatProperties@ structure
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_FORMAT_NOT_SUPPORTED@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkFormat', 'VkImageCreateFlags',
-- 'VkImageFormatProperties', 'VkImageTiling', 'VkImageType',
-- 'VkImageUsageFlags', 'VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
-- | VkExtent3D - Structure specifying a three-dimensional extent
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageCopy',
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'VkImageFormatProperties',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageResolve',
-- 'VkQueueFamilyProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind'
data VkExtent3D = VkExtent3D
  { -- | @width@ is the width of the extent.
  vkWidth :: Word32
  , -- | @height@ is the height of the extent.
  vkHeight :: Word32
  , -- | @depth@ is the depth of the extent.
  vkDepth :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkExtent3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkWidth (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 4) (vkHeight (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 8) (vkDepth (poked :: VkExtent3D))
-- | VkPhysicalDeviceProperties - Structure specifying physical device
-- properties
--
-- = Description
--
-- The @vendorID@ and @deviceID@ fields are provided to allow applications
-- to adapt to device characteristics that are not adequately exposed by
-- other Vulkan queries.
--
-- __Note__
--
-- These /may/ include performance profiles, hardware errata, or other
-- characteristics.
--
-- The /vendor/ identified by @vendorID@ is the entity responsible for the
-- most salient characteristics of the underlying implementation of the
-- 'VkPhysicalDevice' being queried.
--
-- __Note__
--
-- For example, in the case of a discrete GPU implementation, this /should/
-- be the GPU chipset vendor. In the case of a hardware accelerator
-- integrated into a system-on-chip (SoC), this /should/ be the supplier of
-- the silicon IP used to create the accelerator.
--
-- If the vendor has a [PCI vendor
-- ID](https://pcisig.com/membership/member-companies), the low 16 bits of
-- @vendorID@ /must/ contain that PCI vendor ID, and the remaining bits
-- /must/ be set to zero. Otherwise, the value returned /must/ be a valid
-- Khronos vendor ID, obtained as described in the [Vulkan Documentation
-- and
-- Extensions](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vulkan-styleguide)
-- document in the section “Registering a Vendor ID with Khronos”. Khronos
-- vendor IDs are allocated starting at 0x10000, to distinguish them from
-- the PCI vendor ID namespace.
--
-- The vendor is also responsible for the value returned in @deviceID@. If
-- the implementation is driven primarily by a [PCI
-- device](https://pcisig.com/) with a [PCI device
-- ID](https://pcisig.com/), the low 16 bits of @deviceID@ /must/ contain
-- that PCI device ID, and the remaining bits /must/ be set to zero.
-- Otherwise, the choice of what values to return /may/ be dictated by
-- operating system or platform policies - but /should/ uniquely identify
-- both the device version and any major configuration options (for
-- example, core count in the case of multicore devices).
--
-- __Note__
--
-- The same device ID /should/ be used for all physical implementations of
-- that device version and configuration. For example, all uses of a
-- specific silicon IP GPU version and configuration /should/ use the same
-- device ID, even if those uses occur in different SoCs.
--
-- = See Also
--
-- 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- 'VkPhysicalDeviceSparseProperties', 'VkPhysicalDeviceType',
-- 'vkGetPhysicalDeviceProperties'
data VkPhysicalDeviceProperties = VkPhysicalDeviceProperties
  { -- | @apiVersion@ is the version of Vulkan supported by the device, encoded
  -- as described in the [API Version Numbers and
  -- Semantics](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-versionnum)
  -- section.
  vkApiVersion :: Word32
  , -- | @driverVersion@ is the vendor-specified version of the driver.
  vkDriverVersion :: Word32
  , -- | @vendorID@ is a unique identifier for the /vendor/ (see below) of the
  -- physical device.
  vkVendorID :: Word32
  , -- | @deviceID@ is a unique identifier for the physical device among devices
  -- available from the vendor.
  vkDeviceID :: Word32
  , -- | @deviceType@ is a 'VkPhysicalDeviceType' specifying the type of device.
  vkDeviceType :: VkPhysicalDeviceType
  , -- | @deviceName@ is a null-terminated UTF-8 string containing the name of
  -- the device.
  vkDeviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar
  , -- | @pipelineCacheUUID@ is an array of size @VK_UUID_SIZE@, containing 8-bit
  -- values that represent a universally unique identifier for the device.
  vkPipelineCacheUUID :: Vector VK_UUID_SIZE Word8
  , -- | @limits@ is the 'VkPhysicalDeviceLimits' structure which specifies
  -- device-specific limits of the physical device. See
  -- [Limits](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits)
  -- for details.
  vkLimits :: VkPhysicalDeviceLimits
  , -- | @sparseProperties@ is the 'VkPhysicalDeviceSparseProperties' structure
  -- which specifies various sparse related properties of the physical
  -- device. See [Sparse
  -- Properties](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-physicalprops)
  -- for details.
  vkSparseProperties :: VkPhysicalDeviceSparseProperties
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProperties where
  sizeOf ~_ = 824
  alignment ~_ = 8
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
-- | VkApplicationInfo - Structure specifying application info
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_APPLICATION_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @pApplicationName@ is not @NULL@, @pApplicationName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   If @pEngineName@ is not @NULL@, @pEngineName@ /must/ be a
--     null-terminated UTF-8 string
--
-- = See Also
--
-- 'VkInstanceCreateInfo', 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkApplicationInfo = VkApplicationInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pApplicationName@ is @NULL@ or is a pointer to a null-terminated UTF-8
  -- string containing the name of the application.
  vkPApplicationName :: Ptr CChar
  , -- | @applicationVersion@ is an unsigned integer variable containing the
  -- developer-supplied version number of the application.
  vkApplicationVersion :: Word32
  , -- | @pEngineName@ is @NULL@ or is a pointer to a null-terminated UTF-8
  -- string containing the name of the engine (if any) used to create the
  -- application.
  vkPEngineName :: Ptr CChar
  , -- | @engineVersion@ is an unsigned integer variable containing the
  -- developer-supplied version number of the engine used to create the
  -- application.
  vkEngineVersion :: Word32
  , -- | @apiVersion@ is the version of the Vulkan API against which the
  -- application expects to run, encoded as described in the [API Version
  -- Numbers and
  -- Semantics](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-versionnum)
  -- section. If @apiVersion@ is 0 the implementation /must/ ignore it,
  -- otherwise if the implementation does not support the requested
  -- @apiVersion@, or an effective substitute for @apiVersion@, it /must/
  -- return @VK_ERROR_INCOMPATIBLE_DRIVER@. The patch version number
  -- specified in @apiVersion@ is ignored when creating an instance object.
  -- Only the major and minor versions of the instance /must/ match those
  -- requested in @apiVersion@.
  vkApiVersion :: Word32
  }
  deriving (Eq, Show)

instance Storable VkApplicationInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
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
-- | VkAllocationCallbacks - Structure containing callback function pointers
-- for memory allocation
--
-- == Valid Usage
--
-- -   @pfnAllocation@ /must/ be a valid pointer to a valid user-defined
--     'PFN_vkAllocationFunction'
--
-- -   @pfnReallocation@ /must/ be a valid pointer to a valid user-defined
--     'PFN_vkReallocationFunction'
--
-- -   @pfnFree@ /must/ be a valid pointer to a valid user-defined
--     'PFN_vkFreeFunction'
--
-- -   If either of @pfnInternalAllocation@ or @pfnInternalFree@ is not
--     @NULL@, both /must/ be valid callbacks
--
-- = See Also
--
-- 'PFN_vkAllocationFunction', 'PFN_vkFreeFunction',
-- 'PFN_vkInternalAllocationNotification',
-- 'PFN_vkInternalFreeNotification', 'PFN_vkReallocationFunction',
-- 'Graphics.Vulkan.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR',
-- 'Graphics.Vulkan.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.vkCreateDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkCreateDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkCreateDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.Core10.Event.vkCreateEvent',
-- 'Graphics.Vulkan.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.Core10.Pass.vkCreateFramebuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK',
-- 'Graphics.Vulkan.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCreateIndirectCommandsLayoutNVX',
-- 'vkCreateInstance',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_KHR_mir_surface.vkCreateMirSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCreateObjectTableNVX',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.vkCreateQueryPool',
-- 'Graphics.Vulkan.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.Core10.Sampler.vkCreateSampler',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.Core10.Shader.vkCreateShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.vkCreateSharedSwapchainsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.vkCreateViSurfaceNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR',
-- 'Graphics.Vulkan.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.vkDestroyBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.vkDestroyDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkDestroyDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.vkDestroyDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.vkDestroyDevice',
-- 'Graphics.Vulkan.Core10.Event.vkDestroyEvent',
-- 'Graphics.Vulkan.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.Core10.Pass.vkDestroyFramebuffer',
-- 'Graphics.Vulkan.Core10.Image.vkDestroyImage',
-- 'Graphics.Vulkan.Core10.ImageView.vkDestroyImageView',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkDestroyIndirectCommandsLayoutNVX',
-- 'vkDestroyInstance',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkDestroyObjectTableNVX',
-- 'Graphics.Vulkan.Core10.Pipeline.vkDestroyPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.vkDestroyPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.vkDestroyPipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.vkDestroyQueryPool',
-- 'Graphics.Vulkan.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.Core10.Sampler.vkDestroySampler',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.vkDestroySemaphore',
-- 'Graphics.Vulkan.Core10.Shader.vkDestroyShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkDestroySurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkDestroySwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.vkDestroyValidationCacheEXT',
-- 'Graphics.Vulkan.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDeviceEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT'
data VkAllocationCallbacks = VkAllocationCallbacks
  { -- | @pUserData@ is a value to be interpreted by the implementation of the
  -- callbacks. When any of the callbacks in @VkAllocationCallbacks@ are
  -- called, the Vulkan implementation will pass this value as the first
  -- parameter to the callback. This value /can/ vary each time an allocator
  -- is passed into a command, even when the same object takes an allocator
  -- in multiple commands.
  vkPUserData :: Ptr ()
  , -- | @pfnAllocation@ is a pointer to an application-defined memory allocation
  -- function of type 'PFN_vkAllocationFunction'.
  vkPfnAllocation :: PFN_vkAllocationFunction
  , -- | @pfnReallocation@ is a pointer to an application-defined memory
  -- reallocation function of type 'PFN_vkReallocationFunction'.
  vkPfnReallocation :: PFN_vkReallocationFunction
  , -- | @pfnFree@ is a pointer to an application-defined memory free function of
  -- type 'PFN_vkFreeFunction'.
  vkPfnFree :: PFN_vkFreeFunction
  , -- | @pfnInternalAllocation@ is a pointer to an application-defined function
  -- that is called by the implementation when the implementation makes
  -- internal allocations, and it is of type
  -- 'PFN_vkInternalAllocationNotification'.
  vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- | @pfnInternalFree@ is a pointer to an application-defined function that
  -- is called by the implementation when the implementation frees internal
  -- allocations, and it is of type 'PFN_vkInternalFreeNotification'.
  vkPfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Eq, Show)

instance Storable VkAllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
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
-- | VkInstanceCreateInfo - Structure specifying parameters of a newly
-- created instance
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT',
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_validation_flags.VkValidationFlagsEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   If @pApplicationInfo@ is not @NULL@, @pApplicationInfo@ /must/ be a
--     valid pointer to a valid @VkApplicationInfo@ structure
--
-- -   If @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   If @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@
--     /must/ be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- = See Also
--
-- 'VkApplicationInfo', 'VkInstanceCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateInstance'
data VkInstanceCreateInfo = VkInstanceCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkInstanceCreateFlags
  , -- | @pApplicationInfo@ is @NULL@ or a pointer to an instance of
  -- @VkApplicationInfo@. If not @NULL@, this information helps
  -- implementations recognize behavior inherent to classes of applications.
  -- 'VkApplicationInfo' is defined in detail below.
  vkPApplicationInfo :: Ptr VkApplicationInfo
  , -- | @enabledLayerCount@ is the number of global layers to enable.
  vkEnabledLayerCount :: Word32
  , -- | @ppEnabledLayerNames@ is a pointer to an array of @enabledLayerCount@
  -- null-terminated UTF-8 strings containing the names of layers to enable
  -- for the created instance. See the
  -- [Layers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#extended-functionality-layers)
  -- section for further details.
  vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , -- | @enabledExtensionCount@ is the number of global extensions to enable.
  vkEnabledExtensionCount :: Word32
  , -- | @ppEnabledExtensionNames@ is a pointer to an array of
  -- @enabledExtensionCount@ null-terminated UTF-8 strings containing the
  -- names of extensions to enable.
  vkPPEnabledExtensionNames :: Ptr (Ptr CChar)
  }
  deriving (Eq, Show)

instance Storable VkInstanceCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
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
                *> poke (ptr `plusPtr` 40) (vkPPEnabledLayerNames (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPPEnabledExtensionNames (poked :: VkInstanceCreateInfo))
-- | VkQueueFamilyProperties - Structure providing information about a queue
-- family
--
-- = Description
--
-- The value returned in @minImageTransferGranularity@ has a unit of
-- compressed texel blocks for images having a block-compressed format, and
-- a unit of texels otherwise.
--
-- Possible values of @minImageTransferGranularity@ are:
--
-- -   (0,0,0) which indicates that only whole mip levels /must/ be
--     transferred using the image transfer operations on the corresponding
--     queues. In this case, the following restrictions apply to all offset
--     and extent parameters of image transfer operations:
--
--     -   The @x@, @y@, and @z@ members of a
--         'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D'
--         parameter /must/ always be zero.
--
--     -   The @width@, @height@, and @depth@ members of a 'VkExtent3D'
--         parameter /must/ always match the width, height, and depth of
--         the image subresource corresponding to the parameter,
--         respectively.
--
-- -   (Ax, Ay, Az) where Ax, Ay, and Az are all integer powers of two. In
--     this case the following restrictions apply to all image transfer
--     operations:
--
--     -   @x@, @y@, and @z@ of a
--         'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D'
--         parameter /must/ be integer multiples of Ax, Ay, and Az,
--         respectively.
--
--     -   @width@ of a 'VkExtent3D' parameter /must/ be an integer
--         multiple of Ax, or else @x@ + @width@ /must/ equal the width of
--         the image subresource corresponding to the parameter.
--
--     -   @height@ of a 'VkExtent3D' parameter /must/ be an integer
--         multiple of Ay, or else @y@ + @height@ /must/ equal the height
--         of the image subresource corresponding to the parameter.
--
--     -   @depth@ of a 'VkExtent3D' parameter /must/ be an integer
--         multiple of Az, or else @z@ + @depth@ /must/ equal the depth of
--         the image subresource corresponding to the parameter.
--
--     -   If the format of the image corresponding to the parameters is
--         one of the block-compressed formats then for the purposes of the
--         above calculations the granularity /must/ be scaled up by the
--         compressed texel block dimensions.
--
-- Queues supporting graphics and\/or compute operations /must/ report
-- (1,1,1) in @minImageTransferGranularity@, meaning that there are no
-- additional restrictions on the granularity of image transfer operations
-- for these queues. Other queues supporting image transfer operations are
-- only /required/ to support whole mip level transfers, thus
-- @minImageTransferGranularity@ for queues belonging to such queue
-- families /may/ be (0,0,0).
--
-- The [Device
-- Memory](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device)
-- section describes memory properties queried from the physical device.
--
-- For physical device feature queries see the
-- [Features](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features)
-- chapter.
--
-- = See Also
--
-- 'VkExtent3D',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2',
-- 'VkQueueFlags', 'vkGetPhysicalDeviceQueueFamilyProperties'
data VkQueueFamilyProperties = VkQueueFamilyProperties
  { -- | @queueFlags@ is a bitmask of 'VkQueueFlagBits' indicating capabilities
  -- of the queues in this queue family.
  vkQueueFlags :: VkQueueFlags
  , -- | @queueCount@ is the unsigned integer count of queues in this queue
  -- family.
  vkQueueCount :: Word32
  , -- | @timestampValidBits@ is the unsigned integer count of meaningful bits in
  -- the timestamps written via @vkCmdWriteTimestamp@. The valid range for
  -- the count is 36..64 bits, or a value of 0, indicating no support for
  -- timestamps. Bits outside the valid range are guaranteed to be zeros.
  vkTimestampValidBits :: Word32
  , -- | @minImageTransferGranularity@ is the minimum granularity supported for
  -- image transfer operations on the queues in this queue family.
  vkMinImageTransferGranularity :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkQueueFamilyProperties where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkQueueFamilyProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkQueueFlags (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 4) (vkQueueCount (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 8) (vkTimestampValidBits (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 12) (vkMinImageTransferGranularity (poked :: VkQueueFamilyProperties))
-- | VkPhysicalDeviceMemoryProperties - Structure specifying physical device
-- memory properties
--
-- = Description
--
-- The @VkPhysicalDeviceMemoryProperties@ structure describes a number of
-- /memory heaps/ as well as a number of /memory types/ that /can/ be used
-- to access memory allocated in those heaps. Each heap describes a memory
-- resource of a particular size, and each memory type describes a set of
-- memory properties (e.g. host cached vs uncached) that /can/ be used with
-- a given memory heap. Allocations using a particular memory type will
-- consume resources from the heap indicated by that memory type’s heap
-- index. More than one memory type /may/ share each heap, and the heaps
-- and memory types provide a mechanism to advertise an accurate size of
-- the physical memory resources while allowing the memory to be used with
-- a variety of different properties.
--
-- The number of memory heaps is given by @memoryHeapCount@ and is less
-- than or equal to @VK_MAX_MEMORY_HEAPS@. Each heap is described by an
-- element of the @memoryHeaps@ array as a 'VkMemoryHeap' structure. The
-- number of memory types available across all memory heaps is given by
-- @memoryTypeCount@ and is less than or equal to @VK_MAX_MEMORY_TYPES@.
-- Each memory type is described by an element of the @memoryTypes@ array
-- as a 'VkMemoryType' structure.
--
-- At least one heap /must/ include @VK_MEMORY_HEAP_DEVICE_LOCAL_BIT@ in
-- 'VkMemoryHeap'::@flags@. If there are multiple heaps that all have
-- similar performance characteristics, they /may/ all include
-- @VK_MEMORY_HEAP_DEVICE_LOCAL_BIT@. In a unified memory architecture
-- (UMA) system there is often only a single memory heap which is
-- considered to be equally “local” to the host and to the device, and such
-- an implementation /must/ advertise the heap as device-local.
--
-- Each memory type returned by 'vkGetPhysicalDeviceMemoryProperties'
-- /must/ have its @propertyFlags@ set to one of the following values:
--
-- -   0
--
-- -   @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@
--
-- -   @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_CACHED_BIT@
--
-- -   @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_CACHED_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@
--
-- -   @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@
--
-- -   @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@
--
-- -   @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_CACHED_BIT@
--
-- -   @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_CACHED_BIT@ |
--     @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@
--
-- -   @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ |
--     @VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT@
--
-- There /must/ be at least one memory type with both the
-- @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ and
-- @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@ bits set in its @propertyFlags@.
-- There /must/ be at least one memory type with the
-- @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ bit set in its @propertyFlags@.
--
-- For each pair of elements __X__ and __Y__ returned in @memoryTypes@,
-- __X__ /must/ be placed at a lower index position than __Y__ if:
--
-- -   either the set of bit flags returned in the @propertyFlags@ member
--     of __X__ is a strict subset of the set of bit flags returned in the
--     @propertyFlags@ member of __Y__.
--
-- -   or the @propertyFlags@ members of __X__ and __Y__ are equal, and
--     __X__ belongs to a memory heap with greater performance (as
--     determined in an implementation-specific manner).
--
-- __Note__
--
-- There is no ordering requirement between __X__ and __Y__ elements for
-- the case their @propertyFlags@ members are not in a subset relation.
-- That potentially allows more than one possible way to order the same set
-- of memory types. Notice that the [list of all allowed memory property
-- flag
-- combinations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device-bitmask-list)
-- is written in the required order. But if instead
-- @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@ was before
-- @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ |
-- @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@, the list would still be in the
-- required order.
--
-- This ordering requirement enables applications to use a simple search
-- loop to select the desired memory type along the lines of:
--
-- > // Find a memory in `memoryTypeBitsRequirement` that includes all of `requiredProperties`
-- > int32_t findProperties(const VkPhysicalDeviceMemoryProperties* pMemoryProperties,
-- >                        uint32_t memoryTypeBitsRequirement,
-- >                        VkMemoryPropertyFlags requiredProperties) {
-- >     const uint32_t memoryCount = pMemoryProperties->memoryTypeCount;
-- >     for (uint32_t memoryIndex = 0; memoryIndex < memoryCount; ++memoryIndex) {
-- >         const uint32_t memoryTypeBits = (1 << memoryIndex);
-- >         const bool isRequiredMemoryType = memoryTypeBitsRequirement & memoryTypeBits;
-- >
-- >         const VkMemoryPropertyFlags properties =
-- >             pMemoryProperties->memoryTypes[memoryIndex].propertyFlags;
-- >         const bool hasRequiredProperties =
-- >             (properties & requiredProperties) == requiredProperties;
-- >
-- >         if (isRequiredMemoryType && hasRequiredProperties)
-- >             return static_cast<int32_t>(memoryIndex);
-- >     }
-- >
-- >     // failed to find memory type
-- >     return -1;
-- > }
-- >
-- > // Try to find an optimal memory type, or if it does not exist try fallback memory type
-- > // `device` is the VkDevice
-- > // `image` is the VkImage that requires memory to be bound
-- > // `memoryProperties` properties as returned by vkGetPhysicalDeviceMemoryProperties
-- > // `requiredProperties` are the property flags that must be present
-- > // `optimalProperties` are the property flags that are preferred by the application
-- > VkMemoryRequirements memoryRequirements;
-- > vkGetImageMemoryRequirements(device, image, &memoryRequirements);
-- > int32_t memoryType =
-- >     findProperties(&memoryProperties, memoryRequirements.memoryTypeBits, optimalProperties);
-- > if (memoryType == -1) // not found; try fallback properties
-- >     memoryType =
-- >         findProperties(&memoryProperties, memoryRequirements.memoryTypeBits, requiredProperties);
--
-- = See Also
--
-- 'VkMemoryHeap', 'VkMemoryType',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2',
-- 'vkGetPhysicalDeviceMemoryProperties'
data VkPhysicalDeviceMemoryProperties = VkPhysicalDeviceMemoryProperties
  { -- | @memoryTypeCount@ is the number of valid elements in the @memoryTypes@
  -- array.
  vkMemoryTypeCount :: Word32
  , -- | @memoryTypes@ is an array of 'VkMemoryType' structures describing the
  -- /memory types/ that /can/ be used to access memory allocated from the
  -- heaps specified by @memoryHeaps@.
  vkMemoryTypes :: Vector VK_MAX_MEMORY_TYPES VkMemoryType
  , -- | @memoryHeapCount@ is the number of valid elements in the @memoryHeaps@
  -- array.
  vkMemoryHeapCount :: Word32
  , -- | @memoryHeaps@ is an array of 'VkMemoryHeap' structures describing the
  -- /memory heaps/ from which memory /can/ be allocated.
  vkMemoryHeaps :: Vector VK_MAX_MEMORY_HEAPS VkMemoryHeap
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryProperties where
  sizeOf ~_ = 520
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 260)
                                              <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMemoryTypeCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 4) (vkMemoryTypes (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 260) (vkMemoryHeapCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 264) (vkMemoryHeaps (poked :: VkPhysicalDeviceMemoryProperties))
-- | VkMemoryType - Structure specifying memory type
--
-- = See Also
--
-- 'VkMemoryPropertyFlags', 'VkPhysicalDeviceMemoryProperties'
data VkMemoryType = VkMemoryType
  { -- | @propertyFlags@ is a bitmask of 'VkMemoryPropertyFlagBits' of properties
  -- for this memory type.
  vkPropertyFlags :: VkMemoryPropertyFlags
  , -- | @heapIndex@ describes which memory heap this memory type corresponds to,
  -- and /must/ be less than @memoryHeapCount@ from the
  -- 'VkPhysicalDeviceMemoryProperties' structure.
  vkHeapIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkMemoryType <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPropertyFlags (poked :: VkMemoryType))
                *> poke (ptr `plusPtr` 4) (vkHeapIndex (poked :: VkMemoryType))
-- | VkMemoryHeap - Structure specifying a memory heap
--
-- = See Also
--
-- @VkDeviceSize@, 'VkMemoryHeapFlags', 'VkPhysicalDeviceMemoryProperties'
data VkMemoryHeap = VkMemoryHeap
  { -- | @size@ is the total memory size in bytes in the heap.
  vkSize :: VkDeviceSize
  , -- | @flags@ is a bitmask of 'VkMemoryHeapFlagBits' specifying attribute
  -- flags for the heap.
  vkFlags :: VkMemoryHeapFlags
  }
  deriving (Eq, Show)

instance Storable VkMemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkMemoryHeap <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryHeap))
                *> poke (ptr `plusPtr` 8) (vkFlags (poked :: VkMemoryHeap))
-- | VkFormatProperties - Structure specifying image format properties
--
-- = Description
--
-- __Note__
--
-- If no format feature flags are supported, the format itself is not
-- supported, and images of that format cannot be created.
--
-- If @format@ is a block-compression format, then buffers /must/ not
-- support any features for the format.
--
-- = See Also
--
-- 'VkFormatFeatureFlags',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2',
-- 'vkGetPhysicalDeviceFormatProperties'
data VkFormatProperties = VkFormatProperties
  { -- | @linearTilingFeatures@ is a bitmask of 'VkFormatFeatureFlagBits'
  -- specifying features supported by images created with a @tiling@
  -- parameter of @VK_IMAGE_TILING_LINEAR@.
  vkLinearTilingFeatures :: VkFormatFeatureFlags
  , -- | @optimalTilingFeatures@ is a bitmask of 'VkFormatFeatureFlagBits'
  -- specifying features supported by images created with a @tiling@
  -- parameter of @VK_IMAGE_TILING_OPTIMAL@.
  vkOptimalTilingFeatures :: VkFormatFeatureFlags
  , -- | @bufferFeatures@ is a bitmask of 'VkFormatFeatureFlagBits' specifying
  -- features supported by buffers.
  vkBufferFeatures :: VkFormatFeatureFlags
  }
  deriving (Eq, Show)

instance Storable VkFormatProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkFormatProperties <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLinearTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkOptimalTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkBufferFeatures (poked :: VkFormatProperties))
-- | VkImageFormatProperties - Structure specifying a image format properties
--
-- = Members
--
-- -   @maxExtent@ are the maximum image dimensions. See the [Allowed
--     Extent
--     Values](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-extentperimagetype)
--     section below for how these values are constrained by @type@.
--
-- -   @maxMipLevels@ is the maximum number of mipmap levels.
--     @maxMipLevels@ /must/ be equal to ⌈log2(max(@width@, @height@,
--     @depth@))⌉ + 1, where @width@, @height@, and @depth@ are taken from
--     the corresponding members of @maxExtent@, except when one of the
--     following conditions is true, in which case it /may/ instead be @1@:
--
--     -   @vkGetPhysicalDeviceImageFormatProperties@::@tiling@ was
--         @VK_IMAGE_TILING_LINEAR@
--
--     -   the
--         'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'::@pNext@
--         chain included an instance of
--         'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--         with a handle type included in the @handleTypes@ member for
--         which mipmap image support is not required
--
-- -   @maxArrayLayers@ is the maximum number of array layers.
--     @maxArrayLayers@ /must/ either be equal to 1 or be greater than or
--     equal to the @maxImageArrayLayers@ member of
--     'VkPhysicalDeviceLimits'. A value of 1 is valid only if @tiling@ is
--     @VK_IMAGE_TILING_LINEAR@ or if @type@ is @VK_IMAGE_TYPE_3D@.
--
-- -   @sampleCounts@ is a bitmask of 'VkSampleCountFlagBits' specifying
--     all the supported sample counts for this image as described
--     [below](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-supported-sample-counts).
--
-- -   @maxResourceSize@ is an upper bound on the total image size in
--     bytes, inclusive of all image subresources. Implementations /may/
--     have an address space limit on total size of a resource, which is
--     advertised by this property. @maxResourceSize@ /must/ be at least
--     231.
--
-- = Description
--
-- __Note__
--
-- There is no mechanism to query the size of an image before creating it,
-- to compare that size against @maxResourceSize@. If an application
-- attempts to create an image that exceeds this limit, the creation will
-- fail and 'Graphics.Vulkan.Core10.Image.vkCreateImage' will return
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@. While the advertised limit /must/ be at
-- least 231, it /may/ not be possible to create an image that approaches
-- that size, particularly for @VK_IMAGE_TYPE_1D@.
--
-- If the combination of parameters to
-- @vkGetPhysicalDeviceImageFormatProperties@ is not supported by the
-- implementation for use in 'Graphics.Vulkan.Core10.Image.vkCreateImage',
-- then all members of @VkImageFormatProperties@ will be filled with zero.
--
-- __Note__
--
-- Filling @VkImageFormatProperties@ with zero for unsupported formats is
-- an exception to the usual rule that output structures have undefined
-- contents on error. This exception was unintentional, but is preserved
-- for backwards compatibility.
--
-- = See Also
--
-- @VkDeviceSize@, 'VkExtent3D',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2',
-- 'VkSampleCountFlags', 'vkGetPhysicalDeviceImageFormatProperties'
data VkImageFormatProperties = VkImageFormatProperties
  { -- No documentation found for Nested "VkImageFormatProperties" "maxExtent"
  vkMaxExtent :: VkExtent3D
  , -- No documentation found for Nested "VkImageFormatProperties" "maxMipLevels"
  vkMaxMipLevels :: Word32
  , -- No documentation found for Nested "VkImageFormatProperties" "maxArrayLayers"
  vkMaxArrayLayers :: Word32
  , -- No documentation found for Nested "VkImageFormatProperties" "sampleCounts"
  vkSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkImageFormatProperties" "maxResourceSize"
  vkMaxResourceSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
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
-- | VkPhysicalDeviceFeatures - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceFeatures@ structure describe the
-- following features:
--
-- = Description
--
-- -   @robustBufferAccess@ specifies that accesses to buffers are
--     bounds-checked against the range of the buffer descriptor (as
--     determined by @VkDescriptorBufferInfo@::@range@,
--     @VkBufferViewCreateInfo@::@range@, or the size of the buffer). Out
--     of bounds accesses /must/ not cause application termination, and the
--     effects of shader loads, stores, and atomics /must/ conform to an
--     implementation-dependent behavior as described below.
--
--     -   A buffer access is considered to be out of bounds if any of the
--         following are true:
--
--         -   The pointer was formed by @OpImageTexelPointer@ and the
--             coordinate is less than zero or greater than or equal to the
--             number of whole elements in the bound range.
--
--         -   The pointer was not formed by @OpImageTexelPointer@ and the
--             object pointed to is not wholly contained within the bound
--             range. This includes accesses performed via /variable
--             pointers/ where the buffer descriptor being accessed cannot
--             be statically determined. Uninitialized pointers and
--             pointers equal to @OpConstantNull@ are treated as pointing
--             to a zero-sized object, so all accesses through such
--             pointers are considered to be out of bounds.
--
--             __Note__
--
--             If a SPIR-V @OpLoad@ instruction loads a structure and the
--             tail end of the structure is out of bounds, then all members
--             of the structure are considered out of bounds even if the
--             members at the end are not statically used.
--
--         -   If any buffer access in a given SPIR-V block is determined
--             to be out of bounds, then any other access of the same type
--             (load, store, or atomic) in the same SPIR-V block that
--             accesses an address less than 16 bytes away from the out of
--             bounds address /may/ also be considered out of bounds.
--
--     -   Out-of-bounds buffer loads will return any of the following
--         values:
--
--         -   Values from anywhere within the memory range(s) bound to the
--             buffer (possibly including bytes of memory past the end of
--             the buffer, up to the end of the bound range).
--
--         -   Zero values, or (0,0,0,x) vectors for vector reads where x
--             is a valid value represented in the type of the vector
--             components and /may/ be any of:
--
--             -   0, 1, or the maximum representable positive integer
--                 value, for signed or unsigned integer components
--
--             -   0.0 or 1.0, for floating-point components
--
--     -   Out-of-bounds writes /may/ modify values within the memory
--         range(s) bound to the buffer, but /must/ not modify any other
--         memory.
--
--     -   Out-of-bounds atomics /may/ modify values within the memory
--         range(s) bound to the buffer, but /must/ not modify any other
--         memory, and return an undefined value.
--
--     -   Vertex input attributes are considered out of bounds if the
--         offset of the attribute in the bound vertex buffer range plus
--         the size of the attribute is greater than either:
--
--         -   @vertexBufferRangeSize@, if @bindingStride@ == 0; or
--
--         -   (@vertexBufferRangeSize@ - (@vertexBufferRangeSize@ %
--             @bindingStride@))
--
--         where @vertexBufferRangeSize@ is the byte size of the memory
--         range bound to the vertex buffer binding and @bindingStride@ is
--         the byte stride of the corresponding vertex input binding.
--         Further, if any vertex input attribute using a specific vertex
--         input binding is out of bounds, then all vertex input attributes
--         using that vertex input binding for that vertex shader
--         invocation are considered out of bounds.
--
--         -   If a vertex input attribute is out of bounds, it will be
--             assigned one of the following values:
--
--             -   Values from anywhere within the memory range(s) bound to
--                 the buffer, converted according to the format of the
--                 attribute.
--
--             -   Zero values, format converted according to the format of
--                 the attribute.
--
--             -   Zero values, or (0,0,0,x) vectors, as described above.
--
--     -   If @robustBufferAccess@ is not enabled, out of bounds accesses
--         /may/ corrupt any memory within the process and cause undefined
--         behavior up to and including application termination.
--
-- -   @fullDrawIndexUint32@ specifies the full 32-bit range of indices is
--     supported for indexed draw calls when using a
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkIndexType' of
--     @VK_INDEX_TYPE_UINT32@. @maxDrawIndexedIndexValue@ is the maximum
--     index value that /may/ be used (aside from the primitive restart
--     index, which is always 232-1 when the
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkIndexType' is
--     @VK_INDEX_TYPE_UINT32@). If this feature is supported,
--     @maxDrawIndexedIndexValue@ /must/ be 232-1; otherwise it /must/ be
--     no smaller than 224-1. See
--     [maxDrawIndexedIndexValue](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxDrawIndexedIndexValue).
--
-- -   @imageCubeArray@ specifies whether image views with a
--     'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@ /can/ be created, and that the
--     corresponding @SampledCubeArray@ and @ImageCubeArray@ SPIR-V
--     capabilities /can/ be used in shader code.
--
-- -   @independentBlend@ specifies whether the
--     @VkPipelineColorBlendAttachmentState@ settings are controlled
--     independently per-attachment. If this feature is not enabled, the
--     @VkPipelineColorBlendAttachmentState@ settings for all color
--     attachments /must/ be identical. Otherwise, a different
--     @VkPipelineColorBlendAttachmentState@ /can/ be provided for each
--     bound color attachment.
--
-- -   @geometryShader@ specifies whether geometry shaders are supported.
--     If this feature is not enabled, the @VK_SHADER_STAGE_GEOMETRY_BIT@
--     and @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@ enum values /must/ not
--     be used. This also specifies whether shader modules /can/ declare
--     the @Geometry@ capability.
--
-- -   @tessellationShader@ specifies whether tessellation control and
--     evaluation shaders are supported. If this feature is not enabled,
--     the @VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT@,
--     @VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT@,
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@,
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@, and
--     @VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO@ enum
--     values /must/ not be used. This also specifies whether shader
--     modules /can/ declare the @Tessellation@ capability.
--
-- -   @sampleRateShading@ specifies whether [Sample
--     Shading](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-sampleshading)
--     and multisample interpolation are supported. If this feature is not
--     enabled, the @sampleShadingEnable@ member of the
--     @VkPipelineMultisampleStateCreateInfo@ structure /must/ be set to
--     @VK_FALSE@ and the @minSampleShading@ member is ignored. This also
--     specifies whether shader modules /can/ declare the
--     @SampleRateShading@ capability.
--
-- -   @dualSrcBlend@ specifies whether blend operations which take two
--     sources are supported. If this feature is not enabled, the
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, and
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@ enum values /must/ not be
--     used as source or destination blending factors. See
--     [{html_spec_relative}#framebuffer-dsb](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-dsb).
--
-- -   @logicOp@ specifies whether logic operations are supported. If this
--     feature is not enabled, the @logicOpEnable@ member of the
--     @VkPipelineColorBlendStateCreateInfo@ structure /must/ be set to
--     @VK_FALSE@, and the @logicOp@ member is ignored.
--
-- -   @multiDrawIndirect@ specifies whether multiple draw indirect is
--     supported. If this feature is not enabled, the @drawCount@ parameter
--     to the @vkCmdDrawIndirect@ and @vkCmdDrawIndexedIndirect@ commands
--     /must/ be 0 or 1. The @maxDrawIndirectCount@ member of the
--     @VkPhysicalDeviceLimits@ structure /must/ also be 1 if this feature
--     is not supported. See
--     [maxDrawIndirectCount](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxDrawIndirectCount).
--
-- -   @drawIndirectFirstInstance@ specifies whether indirect draw calls
--     support the @firstInstance@ parameter. If this feature is not
--     enabled, the @firstInstance@ member of all @VkDrawIndirectCommand@
--     and @VkDrawIndexedIndirectCommand@ structures that are provided to
--     the @vkCmdDrawIndirect@ and @vkCmdDrawIndexedIndirect@ commands
--     /must/ be 0.
--
-- -   @depthClamp@ specifies whether depth clamping is supported. If this
--     feature is not enabled, the @depthClampEnable@ member of the
--     @VkPipelineRasterizationStateCreateInfo@ structure /must/ be set to
--     @VK_FALSE@. Otherwise, setting @depthClampEnable@ to @VK_TRUE@ will
--     enable depth clamping.
--
-- -   @depthBiasClamp@ specifies whether depth bias clamping is supported.
--     If this feature is not enabled, the @depthBiasClamp@ member of the
--     @VkPipelineRasterizationStateCreateInfo@ structure /must/ be set to
--     0.0 unless the @VK_DYNAMIC_STATE_DEPTH_BIAS@ dynamic state is
--     enabled, and the @depthBiasClamp@ parameter to @vkCmdSetDepthBias@
--     /must/ be set to 0.0.
--
-- -   @fillModeNonSolid@ specifies whether point and wireframe fill modes
--     are supported. If this feature is not enabled, the
--     @VK_POLYGON_MODE_POINT@ and @VK_POLYGON_MODE_LINE@ enum values
--     /must/ not be used.
--
-- -   @depthBounds@ specifies whether depth bounds tests are supported. If
--     this feature is not enabled, the @depthBoundsTestEnable@ member of
--     the @VkPipelineDepthStencilStateCreateInfo@ structure /must/ be set
--     to @VK_FALSE@. When @depthBoundsTestEnable@ is set to @VK_FALSE@,
--     the @minDepthBounds@ and @maxDepthBounds@ members of the
--     @VkPipelineDepthStencilStateCreateInfo@ structure are ignored.
--
-- -   @wideLines@ specifies whether lines with width other than 1.0 are
--     supported. If this feature is not enabled, the @lineWidth@ member of
--     the @VkPipelineRasterizationStateCreateInfo@ structure /must/ be set
--     to 1.0 unless the @VK_DYNAMIC_STATE_LINE_WIDTH@ dynamic state is
--     enabled, and the @lineWidth@ parameter to @vkCmdSetLineWidth@ /must/
--     be set to 1.0. When this feature is supported, the range and
--     granularity of supported line widths are indicated by the
--     @lineWidthRange@ and @lineWidthGranularity@ members of the
--     @VkPhysicalDeviceLimits@ structure, respectively.
--
-- -   @largePoints@ specifies whether points with size greater than 1.0
--     are supported. If this feature is not enabled, only a point size of
--     1.0 written by a shader is supported. The range and granularity of
--     supported point sizes are indicated by the @pointSizeRange@ and
--     @pointSizeGranularity@ members of the @VkPhysicalDeviceLimits@
--     structure, respectively.
--
-- -   @alphaToOne@ specifies whether the implementation is able to replace
--     the alpha value of the color fragment output from the fragment
--     shader with the maximum representable alpha value for fixed-point
--     colors or 1.0 for floating-point colors. If this feature is not
--     enabled, then the @alphaToOneEnable@ member of the
--     @VkPipelineMultisampleStateCreateInfo@ structure /must/ be set to
--     @VK_FALSE@. Otherwise setting @alphaToOneEnable@ to @VK_TRUE@ will
--     enable alpha-to-one behavior.
--
-- -   @multiViewport@ specifies whether more than one viewport is
--     supported. If this feature is not enabled, the @viewportCount@ and
--     @scissorCount@ members of the @VkPipelineViewportStateCreateInfo@
--     structure /must/ be set to 1. Similarly, the @viewportCount@
--     parameter to the @vkCmdSetViewport@ command and the @scissorCount@
--     parameter to the @vkCmdSetScissor@ command /must/ be 1, and the
--     @firstViewport@ parameter to the @vkCmdSetViewport@ command and the
--     @firstScissor@ parameter to the @vkCmdSetScissor@ command /must/ be
--     0.
--
-- -   @samplerAnisotropy@ specifies whether anisotropic filtering is
--     supported. If this feature is not enabled, the @anisotropyEnable@
--     member of the @VkSamplerCreateInfo@ structure /must/ be @VK_FALSE@.
--
-- -   @textureCompressionETC2@ specifies whether all of the ETC2 and EAC
--     compressed texture formats are supported. If this feature is
--     enabled, then the @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@,
--     @VK_FORMAT_FEATURE_BLIT_SRC_BIT@ and
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ features /must/
--     be supported in @optimalTilingFeatures@ for the following formats:
--
--     -   @VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK@
--
--     -   @VK_FORMAT_EAC_R11_UNORM_BLOCK@
--
--     -   @VK_FORMAT_EAC_R11_SNORM_BLOCK@
--
--     -   @VK_FORMAT_EAC_R11G11_UNORM_BLOCK@
--
--     -   @VK_FORMAT_EAC_R11G11_SNORM_BLOCK@
--
--     'vkGetPhysicalDeviceFormatProperties' and
--     'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check
--     for additional supported properties of individual formats.
--
-- -   @textureCompressionASTC_LDR@ specifies whether all of the ASTC LDR
--     compressed texture formats are supported. If this feature is
--     enabled, then the @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@,
--     @VK_FORMAT_FEATURE_BLIT_SRC_BIT@ and
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ features /must/
--     be supported in @optimalTilingFeatures@ for the following formats:
--
--     -   @VK_FORMAT_ASTC_4x4_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_4x4_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_5x4_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_5x4_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_5x5_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_5x5_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_6x5_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_6x5_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_6x6_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_6x6_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_8x5_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_8x5_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_8x6_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_8x6_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_8x8_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_8x8_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x5_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x5_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x6_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x6_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x8_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x8_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x10_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_10x10_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_12x10_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_12x10_SRGB_BLOCK@
--
--     -   @VK_FORMAT_ASTC_12x12_UNORM_BLOCK@
--
--     -   @VK_FORMAT_ASTC_12x12_SRGB_BLOCK@
--
--     'vkGetPhysicalDeviceFormatProperties' and
--     'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check
--     for additional supported properties of individual formats.
--
-- -   @textureCompressionBC@ specifies whether all of the BC compressed
--     texture formats are supported. If this feature is enabled, then the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT@,
--     @VK_FORMAT_FEATURE_BLIT_SRC_BIT@ and
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ features /must/
--     be supported in @optimalTilingFeatures@ for the following formats:
--
--     -   @VK_FORMAT_BC1_RGB_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC1_RGB_SRGB_BLOCK@
--
--     -   @VK_FORMAT_BC1_RGBA_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC1_RGBA_SRGB_BLOCK@
--
--     -   @VK_FORMAT_BC2_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC2_SRGB_BLOCK@
--
--     -   @VK_FORMAT_BC3_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC3_SRGB_BLOCK@
--
--     -   @VK_FORMAT_BC4_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC4_SNORM_BLOCK@
--
--     -   @VK_FORMAT_BC5_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC5_SNORM_BLOCK@
--
--     -   @VK_FORMAT_BC6H_UFLOAT_BLOCK@
--
--     -   @VK_FORMAT_BC6H_SFLOAT_BLOCK@
--
--     -   @VK_FORMAT_BC7_UNORM_BLOCK@
--
--     -   @VK_FORMAT_BC7_SRGB_BLOCK@
--
--     'vkGetPhysicalDeviceFormatProperties' and
--     'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check
--     for additional supported properties of individual formats.
--
-- -   @occlusionQueryPrecise@ specifies whether occlusion queries
--     returning actual sample counts are supported. Occlusion queries are
--     created in a @VkQueryPool@ by specifying the @queryType@ of
--     @VK_QUERY_TYPE_OCCLUSION@ in the @VkQueryPoolCreateInfo@ structure
--     which is passed to @vkCreateQueryPool@. If this feature is enabled,
--     queries of this type /can/ enable @VK_QUERY_CONTROL_PRECISE_BIT@ in
--     the @flags@ parameter to @vkCmdBeginQuery@. If this feature is not
--     supported, the implementation supports only boolean occlusion
--     queries. When any samples are passed, boolean queries will return a
--     non-zero result value, otherwise a result value of zero is returned.
--     When this feature is enabled and @VK_QUERY_CONTROL_PRECISE_BIT@ is
--     set, occlusion queries will report the actual number of samples
--     passed.
--
-- -   @pipelineStatisticsQuery@ specifies whether the pipeline statistics
--     queries are supported. If this feature is not enabled, queries of
--     type @VK_QUERY_TYPE_PIPELINE_STATISTICS@ /cannot/ be created, and
--     none of the
--     'Graphics.Vulkan.Core10.Query.VkQueryPipelineStatisticFlagBits' bits
--     /can/ be set in the @pipelineStatistics@ member of the
--     @VkQueryPoolCreateInfo@ structure.
--
-- -   @vertexPipelineStoresAndAtomics@ specifies whether storage buffers
--     and images support stores and atomic operations in the vertex,
--     tessellation, and geometry shader stages. If this feature is not
--     enabled, all storage image, storage texel buffers, and storage
--     buffer variables used by these stages in shader modules /must/ be
--     decorated with the @NonWriteable@ decoration (or the @readonly@
--     memory qualifier in GLSL).
--
-- -   @fragmentStoresAndAtomics@ specifies whether storage buffers and
--     images support stores and atomic operations in the fragment shader
--     stage. If this feature is not enabled, all storage image, storage
--     texel buffers, and storage buffer variables used by the fragment
--     stage in shader modules /must/ be decorated with the @NonWriteable@
--     decoration (or the @readonly@ memory qualifier in GLSL).
--
-- -   @shaderTessellationAndGeometryPointSize@ specifies whether the
--     @PointSize@ built-in decoration is available in the tessellation
--     control, tessellation evaluation, and geometry shader stages. If
--     this feature is not enabled, members decorated with the @PointSize@
--     built-in decoration /must/ not be read from or written to and all
--     points written from a tessellation or geometry shader will have a
--     size of 1.0. This also specifies whether shader modules /can/
--     declare the @TessellationPointSize@ capability for tessellation
--     control and evaluation shaders, or if the shader modules /can/
--     declare the @GeometryPointSize@ capability for geometry shaders. An
--     implementation supporting this feature /must/ also support one or
--     both of the
--     [@tessellationShader@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     or
--     [@geometryShader@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     features.
--
-- -   @shaderImageGatherExtended@ specifies whether the extended set of
--     image gather instructions are available in shader code. If this
--     feature is not enabled, the @OpImage@*@Gather@ instructions do not
--     support the @Offset@ and @ConstOffsets@ operands. This also
--     specifies whether shader modules /can/ declare the
--     @ImageGatherExtended@ capability.
--
-- -   @shaderStorageImageExtendedFormats@ specifies whether the extended
--     storage image formats are available in shader code. If this feature
--     is not enabled, the formats requiring the
--     @StorageImageExtendedFormats@ capability are not supported for
--     storage images. This also specifies whether shader modules /can/
--     declare the @StorageImageExtendedFormats@ capability.
--
-- -   @shaderStorageImageMultisample@ specifies whether multisampled
--     storage images are supported. If this feature is not enabled, images
--     that are created with a @usage@ that includes
--     @VK_IMAGE_USAGE_STORAGE_BIT@ /must/ be created with @samples@ equal
--     to @VK_SAMPLE_COUNT_1_BIT@. This also specifies whether shader
--     modules /can/ declare the @StorageImageMultisample@ capability.
--
-- -   @shaderStorageImageReadWithoutFormat@ specifies whether storage
--     images require a format qualifier to be specified when reading from
--     storage images. If this feature is not enabled, the @OpImageRead@
--     instruction /must/ not have an @OpTypeImage@ of @Unknown@. This also
--     specifies whether shader modules /can/ declare the
--     @StorageImageReadWithoutFormat@ capability.
--
-- -   @shaderStorageImageWriteWithoutFormat@ specifies whether storage
--     images require a format qualifier to be specified when writing to
--     storage images. If this feature is not enabled, the @OpImageWrite@
--     instruction /must/ not have an @OpTypeImage@ of @Unknown@. This also
--     specifies whether shader modules /can/ declare the
--     @StorageImageWriteWithoutFormat@ capability.
--
-- -   @shaderUniformBufferArrayDynamicIndexing@ specifies whether arrays
--     of uniform buffers /can/ be indexed by /dynamically uniform/ integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ /must/ be indexed only
--     by constant integral expressions when aggregated into arrays in
--     shader code. This also specifies whether shader modules /can/
--     declare the @UniformBufferArrayDynamicIndexing@ capability.
--
-- -   @shaderSampledImageArrayDynamicIndexing@ specifies whether arrays of
--     samplers or sampled images /can/ be indexed by dynamically uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of @VK_DESCRIPTOR_TYPE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, or
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ /must/ be indexed only by
--     constant integral expressions when aggregated into arrays in shader
--     code. This also specifies whether shader modules /can/ declare the
--     @SampledImageArrayDynamicIndexing@ capability.
--
-- -   @shaderStorageBufferArrayDynamicIndexing@ specifies whether arrays
--     of storage buffers /can/ be indexed by dynamically uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ /must/ be indexed only
--     by constant integral expressions when aggregated into arrays in
--     shader code. This also specifies whether shader modules /can/
--     declare the @StorageBufferArrayDynamicIndexing@ capability.
--
-- -   @shaderStorageImageArrayDynamicIndexing@ specifies whether arrays of
--     storage images /can/ be indexed by dynamically uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ /must/ be indexed only by
--     constant integral expressions when aggregated into arrays in shader
--     code. This also specifies whether shader modules /can/ declare the
--     @StorageImageArrayDynamicIndexing@ capability.
--
-- -   @shaderClipDistance@ specifies whether clip distances are supported
--     in shader code. If this feature is not enabled, any members
--     decorated with the @ClipDistance@ built-in decoration /must/ not be
--     read from or written to in shader modules. This also specifies
--     whether shader modules /can/ declare the @ClipDistance@ capability.
--
-- -   @shaderCullDistance@ specifies whether cull distances are supported
--     in shader code. If this feature is not enabled, any members
--     decorated with the @CullDistance@ built-in decoration /must/ not be
--     read from or written to in shader modules. This also specifies
--     whether shader modules /can/ declare the @CullDistance@ capability.
--
-- -   @shaderFloat64@ specifies whether 64-bit floats (doubles) are
--     supported in shader code. If this feature is not enabled, 64-bit
--     floating-point types /must/ not be used in shader code. This also
--     specifies whether shader modules /can/ declare the @Float64@
--     capability.
--
-- -   @shaderInt64@ specifies whether 64-bit integers (signed and
--     unsigned) are supported in shader code. If this feature is not
--     enabled, 64-bit integer types /must/ not be used in shader code.
--     This also specifies whether shader modules /can/ declare the @Int64@
--     capability.
--
-- -   @shaderInt16@ specifies whether 16-bit integers (signed and
--     unsigned) are supported in shader code. If this feature is not
--     enabled, 16-bit integer types /must/ not be used in shader code.
--     This also specifies whether shader modules /can/ declare the @Int16@
--     capability.
--
-- -   @shaderResourceResidency@ specifies whether image operations that
--     return resource residency information are supported in shader code.
--     If this feature is not enabled, the @OpImageSparse@* instructions
--     /must/ not be used in shader code. This also specifies whether
--     shader modules /can/ declare the @SparseResidency@ capability. The
--     feature requires at least one of the @sparseResidency*@ features to
--     be supported.
--
-- -   @shaderResourceMinLod@ specifies whether image operations that
--     specify the minimum resource LOD are supported in shader code. If
--     this feature is not enabled, the @MinLod@ image operand /must/ not
--     be used in shader code. This also specifies whether shader modules
--     /can/ declare the @MinLod@ capability.
--
-- -   @sparseBinding@ specifies whether resource memory /can/ be managed
--     at opaque sparse block level instead of at the object level. If this
--     feature is not enabled, resource memory /must/ be bound only on a
--     per-object basis using the @vkBindBufferMemory@ and
--     @vkBindImageMemory@ commands. In this case, buffers and images
--     /must/ not be created with @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@ and
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@ set in the @flags@ member of
--     the @VkBufferCreateInfo@ and @VkImageCreateInfo@ structures,
--     respectively. Otherwise resource memory /can/ be managed as
--     described in [Sparse Resource
--     Features](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures).
--
-- -   @sparseResidencyBuffer@ specifies whether the device /can/ access
--     partially resident buffers. If this feature is not enabled, buffers
--     /must/ not be created with @VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT@
--     set in the @flags@ member of the @VkBufferCreateInfo@ structure.
--
-- -   @sparseResidencyImage2D@ specifies whether the device /can/ access
--     partially resident 2D images with 1 sample per pixel. If this
--     feature is not enabled, images with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and @samples@ set to @VK_SAMPLE_COUNT_1_BIT@
--     /must/ not be created with @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--     set in the @flags@ member of the @VkImageCreateInfo@ structure.
--
-- -   @sparseResidencyImage3D@ specifies whether the device /can/ access
--     partially resident 3D images. If this feature is not enabled, images
--     with an @imageType@ of @VK_IMAGE_TYPE_3D@ /must/ not be created with
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@ set in the @flags@ member of
--     the @VkImageCreateInfo@ structure.
--
-- -   @sparseResidency2Samples@ specifies whether the physical device
--     /can/ access partially resident 2D images with 2 samples per pixel.
--     If this feature is not enabled, images with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and @samples@ set to @VK_SAMPLE_COUNT_2_BIT@
--     /must/ not be created with @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--     set in the @flags@ member of the @VkImageCreateInfo@ structure.
--
-- -   @sparseResidency4Samples@ specifies whether the physical device
--     /can/ access partially resident 2D images with 4 samples per pixel.
--     If this feature is not enabled, images with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and @samples@ set to @VK_SAMPLE_COUNT_4_BIT@
--     /must/ not be created with @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--     set in the @flags@ member of the @VkImageCreateInfo@ structure.
--
-- -   @sparseResidency8Samples@ specifies whether the physical device
--     /can/ access partially resident 2D images with 8 samples per pixel.
--     If this feature is not enabled, images with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and @samples@ set to @VK_SAMPLE_COUNT_8_BIT@
--     /must/ not be created with @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--     set in the @flags@ member of the @VkImageCreateInfo@ structure.
--
-- -   @sparseResidency16Samples@ specifies whether the physical device
--     /can/ access partially resident 2D images with 16 samples per pixel.
--     If this feature is not enabled, images with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and @samples@ set to @VK_SAMPLE_COUNT_16_BIT@
--     /must/ not be created with @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@
--     set in the @flags@ member of the @VkImageCreateInfo@ structure.
--
-- -   @sparseResidencyAliased@ specifies whether the physical device /can/
--     correctly access data aliased into multiple locations. If this
--     feature is not enabled, the @VK_BUFFER_CREATE_SPARSE_ALIASED_BIT@
--     and @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@ enum values /must/ not be
--     used in @flags@ members of the @VkBufferCreateInfo@ and
--     @VkImageCreateInfo@ structures, respectively.
--
-- -   @variableMultisampleRate@ specifies whether all pipelines that will
--     be bound to a command buffer during a subpass with no attachments
--     /must/ have the same value for
--     @VkPipelineMultisampleStateCreateInfo@::@rasterizationSamples@. If
--     set to @VK_TRUE@, the implementation supports variable multisample
--     rates in a subpass with no attachments. If set to @VK_FALSE@, then
--     all pipelines bound in such a subpass /must/ have the same
--     multisample rate. This has no effect in situations where a subpass
--     uses any attachments.
--
-- -   @inheritedQueries@ specifies whether a secondary command buffer
--     /may/ be executed while a query is active.
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- 'vkGetPhysicalDeviceFeatures'
data VkPhysicalDeviceFeatures = VkPhysicalDeviceFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceFeatures" "robustBufferAccess"
  vkRobustBufferAccess :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "fullDrawIndexUint32"
  vkFullDrawIndexUint32 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "imageCubeArray"
  vkImageCubeArray :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "independentBlend"
  vkIndependentBlend :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "geometryShader"
  vkGeometryShader :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "tessellationShader"
  vkTessellationShader :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sampleRateShading"
  vkSampleRateShading :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "dualSrcBlend"
  vkDualSrcBlend :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "logicOp"
  vkLogicOp :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "multiDrawIndirect"
  vkMultiDrawIndirect :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "drawIndirectFirstInstance"
  vkDrawIndirectFirstInstance :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "depthClamp"
  vkDepthClamp :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "depthBiasClamp"
  vkDepthBiasClamp :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "fillModeNonSolid"
  vkFillModeNonSolid :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "depthBounds"
  vkDepthBounds :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "wideLines"
  vkWideLines :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "largePoints"
  vkLargePoints :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "alphaToOne"
  vkAlphaToOne :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "multiViewport"
  vkMultiViewport :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "samplerAnisotropy"
  vkSamplerAnisotropy :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "textureCompressionETC2"
  vkTextureCompressionETC2 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "textureCompressionASTC_LDR"
  vkTextureCompressionASTC_LDR :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "textureCompressionBC"
  vkTextureCompressionBC :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "occlusionQueryPrecise"
  vkOcclusionQueryPrecise :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "pipelineStatisticsQuery"
  vkPipelineStatisticsQuery :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "vertexPipelineStoresAndAtomics"
  vkVertexPipelineStoresAndAtomics :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "fragmentStoresAndAtomics"
  vkFragmentStoresAndAtomics :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize"
  vkShaderTessellationAndGeometryPointSize :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderImageGatherExtended"
  vkShaderImageGatherExtended :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageExtendedFormats"
  vkShaderStorageImageExtendedFormats :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageMultisample"
  vkShaderStorageImageMultisample :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageReadWithoutFormat"
  vkShaderStorageImageReadWithoutFormat :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageWriteWithoutFormat"
  vkShaderStorageImageWriteWithoutFormat :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderUniformBufferArrayDynamicIndexing"
  vkShaderUniformBufferArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderSampledImageArrayDynamicIndexing"
  vkShaderSampledImageArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageBufferArrayDynamicIndexing"
  vkShaderStorageBufferArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageArrayDynamicIndexing"
  vkShaderStorageImageArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderClipDistance"
  vkShaderClipDistance :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderCullDistance"
  vkShaderCullDistance :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderFloat64"
  vkShaderFloat64 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderInt64"
  vkShaderInt64 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderInt16"
  vkShaderInt16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderResourceResidency"
  vkShaderResourceResidency :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderResourceMinLod"
  vkShaderResourceMinLod :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseBinding"
  vkSparseBinding :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyBuffer"
  vkSparseResidencyBuffer :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyImage2D"
  vkSparseResidencyImage2D :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyImage3D"
  vkSparseResidencyImage3D :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency2Samples"
  vkSparseResidency2Samples :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency4Samples"
  vkSparseResidency4Samples :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency8Samples"
  vkSparseResidency8Samples :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency16Samples"
  vkSparseResidency16Samples :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyAliased"
  vkSparseResidencyAliased :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "variableMultisampleRate"
  vkVariableMultisampleRate :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "inheritedQueries"
  vkInheritedQueries :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFeatures where
  sizeOf ~_ = 220
  alignment ~_ = 4
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
                *> poke (ptr `plusPtr` 4) (vkFullDrawIndexUint32 (poked :: VkPhysicalDeviceFeatures))
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
                *> poke (ptr `plusPtr` 80) (vkTextureCompressionETC2 (poked :: VkPhysicalDeviceFeatures))
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
                *> poke (ptr `plusPtr` 156) (vkShaderFloat64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 160) (vkShaderInt64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 164) (vkShaderInt16 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 168) (vkShaderResourceResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 172) (vkShaderResourceMinLod (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 176) (vkSparseBinding (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 180) (vkSparseResidencyBuffer (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 184) (vkSparseResidencyImage2D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 188) (vkSparseResidencyImage3D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 192) (vkSparseResidency2Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 196) (vkSparseResidency4Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 200) (vkSparseResidency8Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 204) (vkSparseResidency16Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 208) (vkSparseResidencyAliased (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 212) (vkVariableMultisampleRate (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 216) (vkInheritedQueries (poked :: VkPhysicalDeviceFeatures))
-- | VkPhysicalDeviceSparseProperties - Structure specifying physical device
-- sparse memory properties
--
-- = See Also
--
-- @VkBool32@, 'VkPhysicalDeviceProperties'
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties
  { -- | @residencyStandard2DBlockShape@ is @VK_TRUE@ if the physical device will
  -- access all single-sample 2D sparse resources using the standard sparse
  -- image block shapes (based on image format), as described in the
  -- [Standard Sparse Image Block Shapes (Single
  -- Sample)](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-sparseblockshapessingle)
  -- table. If this property is not supported the value returned in the
  -- @imageGranularity@ member of the @VkSparseImageFormatProperties@
  -- structure for single-sample 2D images is not /required/ to match the
  -- standard sparse image block dimensions listed in the table.
  vkResidencyStandard2DBlockShape :: VkBool32
  , -- | @residencyStandard2DMultisampleBlockShape@ is @VK_TRUE@ if the physical
  -- device will access all multisample 2D sparse resources using the
  -- standard sparse image block shapes (based on image format), as described
  -- in the [Standard Sparse Image Block Shapes
  -- (MSAA)](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-sparseblockshapesmsaa)
  -- table. If this property is not supported, the value returned in the
  -- @imageGranularity@ member of the @VkSparseImageFormatProperties@
  -- structure for multisample 2D images is not /required/ to match the
  -- standard sparse image block dimensions listed in the table.
  vkResidencyStandard2DMultisampleBlockShape :: VkBool32
  , -- | @residencyStandard3DBlockShape@ is @VK_TRUE@ if the physical device will
  -- access all 3D sparse resources using the standard sparse image block
  -- shapes (based on image format), as described in the [Standard Sparse
  -- Image Block Shapes (Single
  -- Sample)](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#sparsememory-sparseblockshapessingle)
  -- table. If this property is not supported, the value returned in the
  -- @imageGranularity@ member of the @VkSparseImageFormatProperties@
  -- structure for 3D images is not /required/ to match the standard sparse
  -- image block dimensions listed in the table.
  vkResidencyStandard3DBlockShape :: VkBool32
  , -- | @residencyAlignedMipSize@ is @VK_TRUE@ if images with mip level
  -- dimensions that are not integer multiples of the corresponding
  -- dimensions of the sparse image block /may/ be placed in the mip tail. If
  -- this property is not reported, only mip levels with dimensions smaller
  -- than the @imageGranularity@ member of the
  -- @VkSparseImageFormatProperties@ structure will be placed in the mip
  -- tail. If this property is reported the implementation is allowed to
  -- return @VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT@ in the @flags@
  -- member of @VkSparseImageFormatProperties@, indicating that mip level
  -- dimensions that are not integer multiples of the corresponding
  -- dimensions of the sparse image block will be placed in the mip tail.
  vkResidencyAlignedMipSize :: VkBool32
  , -- | @residencyNonResidentStrict@ specifies whether the physical device /can/
  -- consistently access non-resident regions of a resource. If this property
  -- is @VK_TRUE@, access to non-resident regions of resources will be
  -- guaranteed to return values as if the resource were populated with 0;
  -- writes to non-resident regions will be discarded.
  vkResidencyNonResidentStrict :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSparseProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkPhysicalDeviceSparseProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkResidencyStandard2DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 4) (vkResidencyStandard2DMultisampleBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 8) (vkResidencyStandard3DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 12) (vkResidencyAlignedMipSize (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 16) (vkResidencyNonResidentStrict (poked :: VkPhysicalDeviceSparseProperties))
-- | VkPhysicalDeviceLimits - Structure reporting implementation-dependent
-- physical device limits
--
-- = Members
--
-- -   @maxImageDimension1D@ is the maximum dimension (@width@) supported
--     for all images created with an @imageType@ of @VK_IMAGE_TYPE_1D@.
--
-- -   @maxImageDimension2D@ is the maximum dimension (@width@ or @height@)
--     supported for all images created with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and without @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@
--     set in @flags@.
--
-- -   @maxImageDimension3D@ is the maximum dimension (@width@, @height@,
--     or @depth@) supported for all images created with an @imageType@ of
--     @VK_IMAGE_TYPE_3D@.
--
-- -   @maxImageDimensionCube@ is the maximum dimension (@width@ or
--     @height@) supported for all images created with an @imageType@ of
--     @VK_IMAGE_TYPE_2D@ and with @VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT@
--     set in @flags@.
--
-- -   @maxImageArrayLayers@ is the maximum number of layers
--     (@arrayLayers@) for an image.
--
-- -   @maxTexelBufferElements@ is the maximum number of addressable texels
--     for a buffer view created on a buffer which was created with the
--     @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@ or
--     @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@ set in the @usage@ member
--     of the @VkBufferCreateInfo@ structure.
--
-- -   @maxUniformBufferRange@ is the maximum value that /can/ be specified
--     in the @range@ member of any
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo'
--     structures passed to a call to
--     'Graphics.Vulkan.Core10.DescriptorSet.vkUpdateDescriptorSets' for
--     descriptors of type @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@.
--
-- -   @maxStorageBufferRange@ is the maximum value that /can/ be specified
--     in the @range@ member of any
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo'
--     structures passed to a call to
--     'Graphics.Vulkan.Core10.DescriptorSet.vkUpdateDescriptorSets' for
--     descriptors of type @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@.
--
-- -   @maxPushConstantsSize@ is the maximum size, in bytes, of the pool of
--     push constant memory. For each of the push constant ranges indicated
--     by the @pPushConstantRanges@ member of the
--     @VkPipelineLayoutCreateInfo@ structure, (@offset@ + @size@) /must/
--     be less than or equal to this limit.
--
-- -   @maxMemoryAllocationCount@ is the maximum number of device memory
--     allocations, as created by
--     'Graphics.Vulkan.Core10.Memory.vkAllocateMemory', which /can/
--     simultaneously exist.
--
-- -   @maxSamplerAllocationCount@ is the maximum number of sampler
--     objects, as created by
--     'Graphics.Vulkan.Core10.Sampler.vkCreateSampler', which /can/
--     simultaneously exist on a device.
--
-- -   @bufferImageGranularity@ is the granularity, in bytes, at which
--     buffer or linear image resources, and optimal image resources /can/
--     be bound to adjacent offsets in the same @VkDeviceMemory@ object
--     without aliasing. See [Buffer-Image
--     Granularity](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-bufferimagegranularity)
--     for more details.
--
-- -   @sparseAddressSpaceSize@ is the total amount of address space
--     available, in bytes, for sparse memory resources. This is an upper
--     bound on the sum of the size of all sparse resources, regardless of
--     whether any memory is bound to them.
--
-- -   @maxBoundDescriptorSets@ is the maximum number of descriptor sets
--     that /can/ be simultaneously used by a pipeline. All @DescriptorSet@
--     decorations in shader modules /must/ have a value less than
--     @maxBoundDescriptorSets@. See
--     [{html_spec_relative}#descriptorsets-sets](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sets).
--
-- -   @maxPerStageDescriptorSamplers@ is the maximum number of samplers
--     that /can/ be accessible to a single shader stage in a pipeline
--     layout. Descriptors with a type of @VK_DESCRIPTOR_TYPE_SAMPLER@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. A descriptor is accessible to a
--     shader stage when the @stageFlags@ member of the
--     @VkDescriptorSetLayoutBinding@ structure has the bit for that shader
--     stage set. See
--     [{html_spec_relative}#descriptorsets-sampler](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampler)
--     and
--     [{html_spec_relative}#descriptorsets-combinedimagesampler](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-combinedimagesampler).
--
-- -   @maxPerStageDescriptorUniformBuffers@ is the maximum number of
--     uniform buffers that /can/ be accessible to a single shader stage in
--     a pipeline layout. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. A descriptor is accessible to a
--     shader stage when the @stageFlags@ member of the
--     @VkDescriptorSetLayoutBinding@ structure has the bit for that shader
--     stage set. See
--     [{html_spec_relative}#descriptorsets-uniformbuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbuffer)
--     and
--     [{html_spec_relative}#descriptorsets-uniformbufferdynamic](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic).
--
-- -   @maxPerStageDescriptorStorageBuffers@ is the maximum number of
--     storage buffers that /can/ be accessible to a single shader stage in
--     a pipeline layout. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. A descriptor is accessible to a
--     pipeline shader stage when the @stageFlags@ member of the
--     @VkDescriptorSetLayoutBinding@ structure has the bit for that shader
--     stage set. See
--     [{html_spec_relative}#descriptorsets-storagebuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebuffer)
--     and
--     [{html_spec_relative}#descriptorsets-storagebufferdynamic](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic).
--
-- -   @maxPerStageDescriptorSampledImages@ is the maximum number of
--     sampled images that /can/ be accessible to a single shader stage in
--     a pipeline layout. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ count against this limit.
--     Only descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. A descriptor is accessible to a
--     pipeline shader stage when the @stageFlags@ member of the
--     @VkDescriptorSetLayoutBinding@ structure has the bit for that shader
--     stage set. See
--     [{html_spec_relative}#descriptorsets-combinedimagesampler](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-combinedimagesampler),
--     [{html_spec_relative}#descriptorsets-sampledimage](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampledimage),
--     and
--     [{html_spec_relative}#descriptorsets-uniformtexelbuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer).
--
-- -   @maxPerStageDescriptorStorageImages@ is the maximum number of
--     storage images that /can/ be accessible to a single shader stage in
--     a pipeline layout. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ count against this limit.
--     Only descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. A descriptor is accessible to a
--     pipeline shader stage when the @stageFlags@ member of the
--     @VkDescriptorSetLayoutBinding@ structure has the bit for that shader
--     stage set. See
--     [{html_spec_relative}#descriptorsets-storageimage](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storageimage),
--     and
--     [{html_spec_relative}#descriptorsets-storagetexelbuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer).
--
-- -   @maxPerStageDescriptorInputAttachments@ is the maximum number of
--     input attachments that /can/ be accessible to a single shader stage
--     in a pipeline layout. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ count against this limit. Only
--     descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. A descriptor is accessible to a
--     pipeline shader stage when the @stageFlags@ member of the
--     @VkDescriptorSetLayoutBinding@ structure has the bit for that shader
--     stage set. These are only supported for the fragment stage. See
--     [{html_spec_relative}#descriptorsets-inputattachment](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-inputattachment).
--
-- -   @maxPerStageResources@ is the maximum number of resources that /can/
--     be accessible to a single shader stage in a pipeline layout.
--     Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, or
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ count against this limit. Only
--     descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. For the fragment shader stage the
--     framebuffer color attachments also count against this limit.
--
-- -   @maxDescriptorSetSamplers@ is the maximum number of samplers that
--     /can/ be included in descriptor bindings in a pipeline layout across
--     all pipeline shader stages and descriptor set numbers. Descriptors
--     with a type of @VK_DESCRIPTOR_TYPE_SAMPLER@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. See
--     [{html_spec_relative}#descriptorsets-sampler](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampler)
--     and
--     [{html_spec_relative}#descriptorsets-combinedimagesampler](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-combinedimagesampler).
--
-- -   @maxDescriptorSetUniformBuffers@ is the maximum number of uniform
--     buffers that /can/ be included in descriptor bindings in a pipeline
--     layout across all pipeline shader stages and descriptor set numbers.
--     Descriptors with a type of @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. See
--     [{html_spec_relative}#descriptorsets-uniformbuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbuffer)
--     and
--     [{html_spec_relative}#descriptorsets-uniformbufferdynamic](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic).
--
-- -   @maxDescriptorSetUniformBuffersDynamic@ is the maximum number of
--     dynamic uniform buffers that /can/ be included in descriptor
--     bindings in a pipeline layout across all pipeline shader stages and
--     descriptor set numbers. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. See
--     [{html_spec_relative}#descriptorsets-uniformbufferdynamic](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic).
--
-- -   @maxDescriptorSetStorageBuffers@ is the maximum number of storage
--     buffers that /can/ be included in descriptor bindings in a pipeline
--     layout across all pipeline shader stages and descriptor set numbers.
--     Descriptors with a type of @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. See
--     [{html_spec_relative}#descriptorsets-storagebuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebuffer)
--     and
--     [{html_spec_relative}#descriptorsets-storagebufferdynamic](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic).
--
-- -   @maxDescriptorSetStorageBuffersDynamic@ is the maximum number of
--     dynamic storage buffers that /can/ be included in descriptor
--     bindings in a pipeline layout across all pipeline shader stages and
--     descriptor set numbers. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ count against this
--     limit. Only descriptors in descriptor set layouts created without
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     bit set count against this limit. See
--     [{html_spec_relative}#descriptorsets-storagebufferdynamic](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic).
--
-- -   @maxDescriptorSetSampledImages@ is the maximum number of sampled
--     images that /can/ be included in descriptor bindings in a pipeline
--     layout across all pipeline shader stages and descriptor set numbers.
--     Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ count against this limit.
--     Only descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. See
--     [{html_spec_relative}#descriptorsets-combinedimagesampler](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-combinedimagesampler),
--     [{html_spec_relative}#descriptorsets-sampledimage](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampledimage),
--     and
--     [{html_spec_relative}#descriptorsets-uniformtexelbuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer).
--
-- -   @maxDescriptorSetStorageImages@ is the maximum number of storage
--     images that /can/ be included in descriptor bindings in a pipeline
--     layout across all pipeline shader stages and descriptor set numbers.
--     Descriptors with a type of @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ count against this limit.
--     Only descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. See
--     [{html_spec_relative}#descriptorsets-storageimage](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storageimage),
--     and
--     [{html_spec_relative}#descriptorsets-storagetexelbuffer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer).
--
-- -   @maxDescriptorSetInputAttachments@ is the maximum number of input
--     attachments that /can/ be included in descriptor bindings in a
--     pipeline layout across all pipeline shader stages and descriptor set
--     numbers. Descriptors with a type of
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ count against this limit. Only
--     descriptors in descriptor set layouts created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set count against this limit. See
--     [{html_spec_relative}#descriptorsets-inputattachment](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-inputattachment).
--
-- -   @maxVertexInputAttributes@ is the maximum number of vertex input
--     attributes that /can/ be specified for a graphics pipeline. These
--     are described in the array of @VkVertexInputAttributeDescription@
--     structures that are provided at graphics pipeline creation time via
--     the @pVertexAttributeDescriptions@ member of the
--     @VkPipelineVertexInputStateCreateInfo@ structure. See
--     [{html_spec_relative}#fxvertex-attrib](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-attrib)
--     and
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input).
--
-- -   @maxVertexInputBindings@ is the maximum number of vertex buffers
--     that /can/ be specified for providing vertex attributes to a
--     graphics pipeline. These are described in the array of
--     @VkVertexInputBindingDescription@ structures that are provided at
--     graphics pipeline creation time via the @pVertexBindingDescriptions@
--     member of the @VkPipelineVertexInputStateCreateInfo@ structure. The
--     @binding@ member of @VkVertexInputBindingDescription@ /must/ be less
--     than this limit. See
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input).
--
-- -   @maxVertexInputAttributeOffset@ is the maximum vertex input
--     attribute offset that /can/ be added to the vertex input binding
--     stride. The @offset@ member of the
--     @VkVertexInputAttributeDescription@ structure /must/ be less than or
--     equal to this limit. See
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input).
--
-- -   @maxVertexInputBindingStride@ is the maximum vertex input binding
--     stride that /can/ be specified in a vertex input binding. The
--     @stride@ member of the @VkVertexInputBindingDescription@ structure
--     /must/ be less than or equal to this limit. See
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input).
--
-- -   @maxVertexOutputComponents@ is the maximum number of components of
--     output variables which /can/ be output by a vertex shader. See
--     [{html_spec_relative}#shaders-vertex](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-vertex).
--
-- -   @maxTessellationGenerationLevel@ is the maximum tessellation
--     generation level supported by the fixed-function tessellation
--     primitive generator. See
--     [{html_spec_relative}#tessellation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#tessellation).
--
-- -   @maxTessellationPatchSize@ is the maximum patch size, in vertices,
--     of patches that /can/ be processed by the tessellation control
--     shader and tessellation primitive generator. The
--     @patchControlPoints@ member of the
--     @VkPipelineTessellationStateCreateInfo@ structure specified at
--     pipeline creation time and the value provided in the
--     @OutputVertices@ execution mode of shader modules /must/ be less
--     than or equal to this limit. See
--     [{html_spec_relative}#tessellation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#tessellation).
--
-- -   @maxTessellationControlPerVertexInputComponents@ is the maximum
--     number of components of input variables which /can/ be provided as
--     per-vertex inputs to the tessellation control shader stage.
--
-- -   @maxTessellationControlPerVertexOutputComponents@ is the maximum
--     number of components of per-vertex output variables which /can/ be
--     output from the tessellation control shader stage.
--
-- -   @maxTessellationControlPerPatchOutputComponents@ is the maximum
--     number of components of per-patch output variables which /can/ be
--     output from the tessellation control shader stage.
--
-- -   @maxTessellationControlTotalOutputComponents@ is the maximum total
--     number of components of per-vertex and per-patch output variables
--     which /can/ be output from the tessellation control shader stage.
--
-- -   @maxTessellationEvaluationInputComponents@ is the maximum number of
--     components of input variables which /can/ be provided as per-vertex
--     inputs to the tessellation evaluation shader stage.
--
-- -   @maxTessellationEvaluationOutputComponents@ is the maximum number of
--     components of per-vertex output variables which /can/ be output from
--     the tessellation evaluation shader stage.
--
-- -   @maxGeometryShaderInvocations@ is the maximum invocation count
--     supported for instanced geometry shaders. The value provided in the
--     @Invocations@ execution mode of shader modules /must/ be less than
--     or equal to this limit. See
--     [{html_spec_relative}#geometry](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#geometry).
--
-- -   @maxGeometryInputComponents@ is the maximum number of components of
--     input variables which /can/ be provided as inputs to the geometry
--     shader stage.
--
-- -   @maxGeometryOutputComponents@ is the maximum number of components of
--     output variables which /can/ be output from the geometry shader
--     stage.
--
-- -   @maxGeometryOutputVertices@ is the maximum number of vertices which
--     /can/ be emitted by any geometry shader.
--
-- -   @maxGeometryTotalOutputComponents@ is the maximum total number of
--     components of output, across all emitted vertices, which /can/ be
--     output from the geometry shader stage.
--
-- -   @maxFragmentInputComponents@ is the maximum number of components of
--     input variables which /can/ be provided as inputs to the fragment
--     shader stage.
--
-- -   @maxFragmentOutputAttachments@ is the maximum number of output
--     attachments which /can/ be written to by the fragment shader stage.
--
-- -   @maxFragmentDualSrcAttachments@ is the maximum number of output
--     attachments which /can/ be written to by the fragment shader stage
--     when blending is enabled and one of the dual source blend modes is
--     in use. See
--     [{html_spec_relative}#framebuffer-dsb](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-dsb)
--     and
--     [dualSrcBlend](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-dualSrcBlend).
--
-- -   @maxFragmentCombinedOutputResources@ is the total number of storage
--     buffers, storage images, and output buffers which /can/ be used in
--     the fragment shader stage.
--
-- -   @maxComputeSharedMemorySize@ is the maximum total storage size, in
--     bytes, of all variables declared with the @WorkgroupLocal@ storage
--     class in shader modules (or with the @shared@ storage qualifier in
--     GLSL) in the compute shader stage.
--
-- -   @maxComputeWorkGroupCount@[3] is the maximum number of local
--     workgroups that /can/ be dispatched by a single dispatch command.
--     These three values represent the maximum number of local workgroups
--     for the X, Y, and Z dimensions, respectively. The workgroup count
--     parameters to the dispatch commands /must/ be less than or equal to
--     the corresponding limit. See
--     [{html_spec_relative}#dispatch](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#dispatch).
--
-- -   @maxComputeWorkGroupInvocations@ is the maximum total number of
--     compute shader invocations in a single local workgroup. The product
--     of the X, Y, and Z sizes as specified by the @LocalSize@ execution
--     mode in shader modules and by the object decorated by the
--     @WorkgroupSize@ decoration /must/ be less than or equal to this
--     limit.
--
-- -   @maxComputeWorkGroupSize@[3] is the maximum size of a local compute
--     workgroup, per dimension. These three values represent the maximum
--     local workgroup size in the X, Y, and Z dimensions, respectively.
--     The @x@, @y@, and @z@ sizes specified by the @LocalSize@ execution
--     mode and by the object decorated by the @WorkgroupSize@ decoration
--     in shader modules /must/ be less than or equal to the corresponding
--     limit.
--
-- -   @subPixelPrecisionBits@ is the number of bits of subpixel precision
--     in framebuffer coordinates xf and yf. See
--     [{html_spec_relative}#primsrast](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast).
--
-- -   @subTexelPrecisionBits@ is the number of bits of precision in the
--     division along an axis of an image used for minification and
--     magnification filters. 2@subTexelPrecisionBits@ is the actual number
--     of divisions along each axis of the image represented. Sub-texel
--     values calculated during image sampling will snap to these locations
--     when generating the filtered results.
--
-- -   @mipmapPrecisionBits@ is the number of bits of division that the LOD
--     calculation for mipmap fetching get snapped to when determining the
--     contribution from each mip level to the mip filtered results.
--     2@mipmapPrecisionBits@ is the actual number of divisions.
--
--     __Note__
--
--     For example, if this value is 2 bits then when linearly filtering
--     between two levels, each level could: contribute: 0%, 33%, 66%, or
--     100% (this is just an example and the amount of contribution
--     /should/ be covered by different equations in the spec).
--
-- -   @maxDrawIndexedIndexValue@ is the maximum index value that /can/ be
--     used for indexed draw calls when using 32-bit indices. This excludes
--     the primitive restart index value of 0xFFFFFFFF. See
--     [fullDrawIndexUint32](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-fullDrawIndexUint32).
--
-- -   @maxDrawIndirectCount@ is the maximum draw count that is supported
--     for indirect draw calls. See
--     [multiDrawIndirect](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiDrawIndirect).
--
-- -   @maxSamplerLodBias@ is the maximum absolute sampler LOD bias. The
--     sum of the @mipLodBias@ member of the @VkSamplerCreateInfo@
--     structure and the @Bias@ operand of image sampling operations in
--     shader modules (or 0 if no @Bias@ operand is provided to an image
--     sampling operation) are clamped to the range
--     [-@maxSamplerLodBias@,+@maxSamplerLodBias@]. See
--     [{html_spec_relative}#samplers-mipLodBias](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-mipLodBias).
--
-- -   @maxSamplerAnisotropy@ is the maximum degree of sampler anisotropy.
--     The maximum degree of anisotropic filtering used for an image
--     sampling operation is the minimum of the @maxAnisotropy@ member of
--     the @VkSamplerCreateInfo@ structure and this limit. See
--     [{html_spec_relative}#samplers-maxAnisotropy](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-maxAnisotropy).
--
-- -   @maxViewports@ is the maximum number of active viewports. The
--     @viewportCount@ member of the @VkPipelineViewportStateCreateInfo@
--     structure that is provided at pipeline creation /must/ be less than
--     or equal to this limit.
--
-- -   @maxViewportDimensions@[2] are the maximum viewport dimensions in
--     the X (width) and Y (height) dimensions, respectively. The maximum
--     viewport dimensions /must/ be greater than or equal to the largest
--     image which /can/ be created and used as a framebuffer attachment.
--     See [Controlling the
--     Viewport](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vertexpostproc-viewport).
--
-- -   @viewportBoundsRange@[2] is the [minimum, maximum] range that the
--     corners of a viewport /must/ be contained in. This range /must/ be
--     at least [-2 × @size@, 2 × @size@ - 1], where @size@ =
--     max(@maxViewportDimensions@[0], @maxViewportDimensions@[1]). See
--     [Controlling the
--     Viewport](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vertexpostproc-viewport).
--
--     __Note__
--
--     The intent of the @viewportBoundsRange@ limit is to allow a maximum
--     sized viewport to be arbitrarily shifted relative to the output
--     target as long as at least some portion intersects. This would give
--     a bounds limit of [-@size@ + 1, 2 × @size@ - 1] which would allow
--     all possible non-empty-set intersections of the output target and
--     the viewport. Since these numbers are typically powers of two,
--     picking the signed number range using the smallest possible number
--     of bits ends up with the specified range.
--
-- -   @viewportSubPixelBits@ is the number of bits of subpixel precision
--     for viewport bounds. The subpixel precision that floating-point
--     viewport bounds are interpreted at is given by this limit.
--
-- -   @minMemoryMapAlignment@ is the minimum /required/ alignment, in
--     bytes, of host visible memory allocations within the host address
--     space. When mapping a memory allocation with
--     'Graphics.Vulkan.Core10.Memory.vkMapMemory', subtracting @offset@
--     bytes from the returned pointer will always produce an integer
--     multiple of this limit. See
--     [{html_spec_relative}#memory-device-hostaccess](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device-hostaccess).
--
-- -   @minTexelBufferOffsetAlignment@ is the minimum /required/ alignment,
--     in bytes, for the @offset@ member of the @VkBufferViewCreateInfo@
--     structure for texel buffers. When a buffer view is created for a
--     buffer which was created with
--     @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@ or
--     @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@ set in the @usage@ member
--     of the @VkBufferCreateInfo@ structure, the @offset@ /must/ be an
--     integer multiple of this limit.
--
-- -   @minUniformBufferOffsetAlignment@ is the minimum /required/
--     alignment, in bytes, for the @offset@ member of the
--     @VkDescriptorBufferInfo@ structure for uniform buffers. When a
--     descriptor of type @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ is updated, the @offset@
--     /must/ be an integer multiple of this limit. Similarly, dynamic
--     offsets for uniform buffers /must/ be multiples of this limit.
--
-- -   @minStorageBufferOffsetAlignment@ is the minimum /required/
--     alignment, in bytes, for the @offset@ member of the
--     @VkDescriptorBufferInfo@ structure for storage buffers. When a
--     descriptor of type @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ is updated, the @offset@
--     /must/ be an integer multiple of this limit. Similarly, dynamic
--     offsets for storage buffers /must/ be multiples of this limit.
--
-- -   @minTexelOffset@ is the minimum offset value for the @ConstOffset@
--     image operand of any of the @OpImageSample@* or @OpImageFetch@*
--     image instructions.
--
-- -   @maxTexelOffset@ is the maximum offset value for the @ConstOffset@
--     image operand of any of the @OpImageSample@* or @OpImageFetch@*
--     image instructions.
--
-- -   @minTexelGatherOffset@ is the minimum offset value for the @Offset@
--     or @ConstOffsets@ image operands of any of the @OpImage@*@Gather@
--     image instructions.
--
-- -   @maxTexelGatherOffset@ is the maximum offset value for the @Offset@
--     or @ConstOffsets@ image operands of any of the @OpImage@*@Gather@
--     image instructions.
--
-- -   @minInterpolationOffset@ is the minimum negative offset value for
--     the @offset@ operand of the @InterpolateAtOffset@ extended
--     instruction.
--
-- -   @maxInterpolationOffset@ is the maximum positive offset value for
--     the @offset@ operand of the @InterpolateAtOffset@ extended
--     instruction.
--
-- -   @subPixelInterpolationOffsetBits@ is the number of subpixel
--     fractional bits that the @x@ and @y@ offsets to the
--     @InterpolateAtOffset@ extended instruction /may/ be rounded to as
--     fixed-point values.
--
-- -   @maxFramebufferWidth@ is the maximum width for a framebuffer. The
--     @width@ member of the @VkFramebufferCreateInfo@ structure /must/ be
--     less than or equal to this limit.
--
-- -   @maxFramebufferHeight@ is the maximum height for a framebuffer. The
--     @height@ member of the @VkFramebufferCreateInfo@ structure /must/ be
--     less than or equal to this limit.
--
-- -   @maxFramebufferLayers@ is the maximum layer count for a layered
--     framebuffer. The @layers@ member of the @VkFramebufferCreateInfo@
--     structure /must/ be less than or equal to this limit.
--
-- -   @framebufferColorSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the color sample counts that are
--     supported for all framebuffer color attachments with floating- or
--     fixed-point formats. There is no limit that specifies the color
--     sample counts that are supported for all color attachments with
--     integer formats.
--
-- -   @framebufferDepthSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the supported depth sample counts
--     for all framebuffer depth\/stencil attachments, when the format
--     includes a depth component.
--
-- -   @framebufferStencilSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the supported stencil sample
--     counts for all framebuffer depth\/stencil attachments, when the
--     format includes a stencil component.
--
-- -   @framebufferNoAttachmentsSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the supported sample counts for a
--     framebuffer with no attachments.
--
-- -   @maxColorAttachments@ is the maximum number of color attachments
--     that /can/ be used by a subpass in a render pass. The
--     @colorAttachmentCount@ member of the @VkSubpassDescription@
--     structure /must/ be less than or equal to this limit.
--
-- -   @sampledImageColorSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the sample counts supported for
--     all 2D images created with @VK_IMAGE_TILING_OPTIMAL@, @usage@
--     containing @VK_IMAGE_USAGE_SAMPLED_BIT@, and a non-integer color
--     format.
--
-- -   @sampledImageIntegerSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the sample counts supported for
--     all 2D images created with @VK_IMAGE_TILING_OPTIMAL@, @usage@
--     containing @VK_IMAGE_USAGE_SAMPLED_BIT@, and an integer color
--     format.
--
-- -   @sampledImageDepthSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the sample counts supported for
--     all 2D images created with @VK_IMAGE_TILING_OPTIMAL@, @usage@
--     containing @VK_IMAGE_USAGE_SAMPLED_BIT@, and a depth format.
--
-- -   @sampledImageStencilSampleCounts@ is a bitmask1 of
--     'VkSampleCountFlagBits' indicating the sample supported for all 2D
--     images created with @VK_IMAGE_TILING_OPTIMAL@, @usage@ containing
--     @VK_IMAGE_USAGE_SAMPLED_BIT@, and a stencil format.
--
-- -   @storageImageSampleCounts@ is a bitmask1 of 'VkSampleCountFlagBits'
--     indicating the sample counts supported for all 2D images created
--     with @VK_IMAGE_TILING_OPTIMAL@, and @usage@ containing
--     @VK_IMAGE_USAGE_STORAGE_BIT@.
--
-- -   @maxSampleMaskWords@ is the maximum number of array elements of a
--     variable decorated with the @SampleMask@ built-in decoration.
--
-- -   @timestampComputeAndGraphics@ specifies support for timestamps on
--     all graphics and compute queues. If this limit is set to @VK_TRUE@,
--     all queues that advertise the @VK_QUEUE_GRAPHICS_BIT@ or
--     @VK_QUEUE_COMPUTE_BIT@ in the
--     @VkQueueFamilyProperties@::@queueFlags@ support
--     @VkQueueFamilyProperties@::@timestampValidBits@ of at least 36. See
--     [Timestamp
--     Queries](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-timestamps).
--
-- -   @timestampPeriod@ is the number of nanoseconds /required/ for a
--     timestamp query to be incremented by 1. See [Timestamp
--     Queries](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-timestamps).
--
-- -   @maxClipDistances@ is the maximum number of clip distances that
--     /can/ be used in a single shader stage. The size of any array
--     declared with the @ClipDistance@ built-in decoration in a shader
--     module /must/ be less than or equal to this limit.
--
-- -   @maxCullDistances@ is the maximum number of cull distances that
--     /can/ be used in a single shader stage. The size of any array
--     declared with the @CullDistance@ built-in decoration in a shader
--     module /must/ be less than or equal to this limit.
--
-- -   @maxCombinedClipAndCullDistances@ is the maximum combined number of
--     clip and cull distances that /can/ be used in a single shader stage.
--     The sum of the sizes of any pair of arrays declared with the
--     @ClipDistance@ and @CullDistance@ built-in decoration used by a
--     single shader stage in a shader module /must/ be less than or equal
--     to this limit.
--
-- -   @discreteQueuePriorities@ is the number of discrete priorities that
--     /can/ be assigned to a queue based on the value of each member of
--     @VkDeviceQueueCreateInfo@::@pQueuePriorities@. This /must/ be at
--     least 2, and levels /must/ be spread evenly over the range, with at
--     least one level at 1.0, and another at 0.0. See
--     [{html_spec_relative}#devsandqueues-priority](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-priority).
--
-- -   @pointSizeRange@[2] is the range [@minimum@,@maximum@] of supported
--     sizes for points. Values written to variables decorated with the
--     @PointSize@ built-in decoration are clamped to this range.
--
-- -   @lineWidthRange@[2] is the range [@minimum@,@maximum@] of supported
--     widths for lines. Values specified by the @lineWidth@ member of the
--     @VkPipelineRasterizationStateCreateInfo@ or the @lineWidth@
--     parameter to @vkCmdSetLineWidth@ are clamped to this range.
--
-- -   @pointSizeGranularity@ is the granularity of supported point sizes.
--     Not all point sizes in the range defined by @pointSizeRange@ are
--     supported. This limit specifies the granularity (or increment)
--     between successive supported point sizes.
--
-- -   @lineWidthGranularity@ is the granularity of supported line widths.
--     Not all line widths in the range defined by @lineWidthRange@ are
--     supported. This limit specifies the granularity (or increment)
--     between successive supported line widths.
--
-- -   @strictLines@ specifies whether lines are rasterized according to
--     the preferred method of rasterization. If set to @VK_FALSE@, lines
--     /may/ be rasterized under a relaxed set of rules. If set to
--     @VK_TRUE@, lines are rasterized as per the strict definition. See
--     [Basic Line Segment
--     Rasterization](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-lines-basic).
--
-- -   @standardSampleLocations@ specifies whether rasterization uses the
--     standard sample locations as documented in
--     [Multisampling](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-multisampling).
--     If set to @VK_TRUE@, the implementation uses the documented sample
--     locations. If set to @VK_FALSE@, the implementation /may/ use
--     different sample locations.
--
-- -   @optimalBufferCopyOffsetAlignment@ is the optimal buffer offset
--     alignment in bytes for @vkCmdCopyBufferToImage@ and
--     @vkCmdCopyImageToBuffer@. The per texel alignment requirements are
--     enforced, but applications /should/ use the optimal alignment for
--     optimal performance and power use.
--
-- -   @optimalBufferCopyRowPitchAlignment@ is the optimal buffer row pitch
--     alignment in bytes for @vkCmdCopyBufferToImage@ and
--     @vkCmdCopyImageToBuffer@. Row pitch is the number of bytes between
--     texels with the same X coordinate in adjacent rows (Y coordinates
--     differ by one). The per texel alignment requirements are enforced,
--     but applications /should/ use the optimal alignment for optimal
--     performance and power use.
--
-- -   @nonCoherentAtomSize@ is the size and alignment in bytes that bounds
--     concurrent access to [host-mapped device
--     memory](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device-hostaccess).
--
-- -   'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.VkPhysicalDeviceDiscardRectanglePropertiesEXT'::@maxDiscardRectangles@
--     is the maximum number of active discard rectangles. This limit can
--     be queried by setting the @pNext@ pointer from a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'
--     object to an instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.VkPhysicalDeviceDiscardRectanglePropertiesEXT'
--     and using @vkGetPhysicalDeviceProperties2@ to fill out the members.
--
-- -   'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties'::@pointClippingBehavior@
--     defines the clipping behavior of points. This limit can be queried
--     by setting the @pNext@ pointer from a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'
--     object to an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties'
--     and using @vkGetPhysicalDeviceProperties2@ to fill out the members.
--
-- -   @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@::@maxVertexAttribDivisor@
--     is the maximum value of the number of instances that will repeat the
--     value of vertex attribute data when instanced rendering is enabled.
--     This limit can be queried by setting the @pNext@ pointer from a
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'
--     object to an instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT'
--     and using @vkGetPhysicalDeviceProperties2@ to fill out the members.
--
-- = Description
--
-- [1]
--     For all bitmasks of 'VkSampleCountFlagBits', the sample count limits
--     defined above represent the minimum supported sample counts for each
--     image type. Individual images /may/ support additional sample
--     counts, which are queried using
--     'vkGetPhysicalDeviceImageFormatProperties' as described in
--     [Supported Sample
--     Counts](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-supported-sample-counts).
--
-- = See Also
--
-- @VkBool32@, @VkDeviceSize@, 'VkPhysicalDeviceProperties',
-- 'VkSampleCountFlags'
data VkPhysicalDeviceLimits = VkPhysicalDeviceLimits
  { -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimension1D"
  vkMaxImageDimension1D :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimension2D"
  vkMaxImageDimension2D :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimension3D"
  vkMaxImageDimension3D :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimensionCube"
  vkMaxImageDimensionCube :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTexelBufferElements"
  vkMaxTexelBufferElements :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxUniformBufferRange"
  vkMaxUniformBufferRange :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxStorageBufferRange"
  vkMaxStorageBufferRange :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPushConstantsSize"
  vkMaxPushConstantsSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxMemoryAllocationCount"
  vkMaxMemoryAllocationCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSamplerAllocationCount"
  vkMaxSamplerAllocationCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "bufferImageGranularity"
  vkBufferImageGranularity :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sparseAddressSpaceSize"
  vkSparseAddressSpaceSize :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxBoundDescriptorSets"
  vkMaxBoundDescriptorSets :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorSamplers"
  vkMaxPerStageDescriptorSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorUniformBuffers"
  vkMaxPerStageDescriptorUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorStorageBuffers"
  vkMaxPerStageDescriptorStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorSampledImages"
  vkMaxPerStageDescriptorSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorStorageImages"
  vkMaxPerStageDescriptorStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorInputAttachments"
  vkMaxPerStageDescriptorInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageResources"
  vkMaxPerStageResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetSamplers"
  vkMaxDescriptorSetSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetUniformBuffers"
  vkMaxDescriptorSetUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetUniformBuffersDynamic"
  vkMaxDescriptorSetUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetStorageBuffers"
  vkMaxDescriptorSetStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetStorageBuffersDynamic"
  vkMaxDescriptorSetStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetSampledImages"
  vkMaxDescriptorSetSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetStorageImages"
  vkMaxDescriptorSetStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetInputAttachments"
  vkMaxDescriptorSetInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputAttributes"
  vkMaxVertexInputAttributes :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputBindings"
  vkMaxVertexInputBindings :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputAttributeOffset"
  vkMaxVertexInputAttributeOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputBindingStride"
  vkMaxVertexInputBindingStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexOutputComponents"
  vkMaxVertexOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationGenerationLevel"
  vkMaxTessellationGenerationLevel :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationPatchSize"
  vkMaxTessellationPatchSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlPerVertexInputComponents"
  vkMaxTessellationControlPerVertexInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlPerVertexOutputComponents"
  vkMaxTessellationControlPerVertexOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlPerPatchOutputComponents"
  vkMaxTessellationControlPerPatchOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlTotalOutputComponents"
  vkMaxTessellationControlTotalOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationEvaluationInputComponents"
  vkMaxTessellationEvaluationInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationEvaluationOutputComponents"
  vkMaxTessellationEvaluationOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryShaderInvocations"
  vkMaxGeometryShaderInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryInputComponents"
  vkMaxGeometryInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryOutputComponents"
  vkMaxGeometryOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryOutputVertices"
  vkMaxGeometryOutputVertices :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryTotalOutputComponents"
  vkMaxGeometryTotalOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentInputComponents"
  vkMaxFragmentInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentOutputAttachments"
  vkMaxFragmentOutputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentDualSrcAttachments"
  vkMaxFragmentDualSrcAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentCombinedOutputResources"
  vkMaxFragmentCombinedOutputResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeSharedMemorySize"
  vkMaxComputeSharedMemorySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeWorkGroupCount"
  vkMaxComputeWorkGroupCount :: Vector 3 Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeWorkGroupInvocations"
  vkMaxComputeWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeWorkGroupSize"
  vkMaxComputeWorkGroupSize :: Vector 3 Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "subPixelPrecisionBits"
  vkSubPixelPrecisionBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "subTexelPrecisionBits"
  vkSubTexelPrecisionBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "mipmapPrecisionBits"
  vkMipmapPrecisionBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDrawIndexedIndexValue"
  vkMaxDrawIndexedIndexValue :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDrawIndirectCount"
  vkMaxDrawIndirectCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSamplerLodBias"
  vkMaxSamplerLodBias :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSamplerAnisotropy"
  vkMaxSamplerAnisotropy :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxViewports"
  vkMaxViewports :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxViewportDimensions"
  vkMaxViewportDimensions :: Vector 2 Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "viewportBoundsRange"
  vkViewportBoundsRange :: Vector 2 CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "viewportSubPixelBits"
  vkViewportSubPixelBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minMemoryMapAlignment"
  vkMinMemoryMapAlignment :: CSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minTexelBufferOffsetAlignment"
  vkMinTexelBufferOffsetAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minUniformBufferOffsetAlignment"
  vkMinUniformBufferOffsetAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minStorageBufferOffsetAlignment"
  vkMinStorageBufferOffsetAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minTexelOffset"
  vkMinTexelOffset :: Int32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTexelOffset"
  vkMaxTexelOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minTexelGatherOffset"
  vkMinTexelGatherOffset :: Int32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTexelGatherOffset"
  vkMaxTexelGatherOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minInterpolationOffset"
  vkMinInterpolationOffset :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxInterpolationOffset"
  vkMaxInterpolationOffset :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "subPixelInterpolationOffsetBits"
  vkSubPixelInterpolationOffsetBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFramebufferWidth"
  vkMaxFramebufferWidth :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFramebufferHeight"
  vkMaxFramebufferHeight :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFramebufferLayers"
  vkMaxFramebufferLayers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferColorSampleCounts"
  vkFramebufferColorSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferDepthSampleCounts"
  vkFramebufferDepthSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferStencilSampleCounts"
  vkFramebufferStencilSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferNoAttachmentsSampleCounts"
  vkFramebufferNoAttachmentsSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxColorAttachments"
  vkMaxColorAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageColorSampleCounts"
  vkSampledImageColorSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageIntegerSampleCounts"
  vkSampledImageIntegerSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageDepthSampleCounts"
  vkSampledImageDepthSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageStencilSampleCounts"
  vkSampledImageStencilSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "storageImageSampleCounts"
  vkStorageImageSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSampleMaskWords"
  vkMaxSampleMaskWords :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "timestampComputeAndGraphics"
  vkTimestampComputeAndGraphics :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "timestampPeriod"
  vkTimestampPeriod :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxClipDistances"
  vkMaxClipDistances :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxCullDistances"
  vkMaxCullDistances :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxCombinedClipAndCullDistances"
  vkMaxCombinedClipAndCullDistances :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "discreteQueuePriorities"
  vkDiscreteQueuePriorities :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "pointSizeRange"
  vkPointSizeRange :: Vector 2 CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "lineWidthRange"
  vkLineWidthRange :: Vector 2 CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "pointSizeGranularity"
  vkPointSizeGranularity :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "lineWidthGranularity"
  vkLineWidthGranularity :: CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "strictLines"
  vkStrictLines :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "standardSampleLocations"
  vkStandardSampleLocations :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "optimalBufferCopyOffsetAlignment"
  vkOptimalBufferCopyOffsetAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "optimalBufferCopyRowPitchAlignment"
  vkOptimalBufferCopyRowPitchAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "nonCoherentAtomSize"
  vkNonCoherentAtomSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceLimits where
  sizeOf ~_ = 504
  alignment ~_ = 8
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxImageDimension1D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 4) (vkMaxImageDimension2D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 8) (vkMaxImageDimension3D (poked :: VkPhysicalDeviceLimits))
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
-- | VkQueueFlags - Bitmask of VkQueueFlagBits
--
-- = Description
--
-- @VkQueueFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkQueueFlagBits'.
--
-- = See Also
--
-- 'VkQueueFamilyProperties', 'VkQueueFlagBits'
type VkQueueFlags = VkQueueFlagBits
-- | VkMemoryPropertyFlags - Bitmask of VkMemoryPropertyFlagBits
--
-- = Description
--
-- @VkMemoryPropertyFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkMemoryPropertyFlagBits'.
--
-- = See Also
--
-- 'VkMemoryPropertyFlagBits', 'VkMemoryType'
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits
-- | VkMemoryHeapFlags - Bitmask of VkMemoryHeapFlagBits
--
-- = Description
--
-- @VkMemoryHeapFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkMemoryHeapFlagBits'.
--
-- = See Also
--
-- 'VkMemoryHeap', 'VkMemoryHeapFlagBits'
type VkMemoryHeapFlags = VkMemoryHeapFlagBits
-- | VkImageUsageFlags - Bitmask of VkImageUsageFlagBits
--
-- = Description
--
-- @VkImageUsageFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkImageUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'VkImageUsageFlagBits',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.VkSharedPresentSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type VkImageUsageFlags = VkImageUsageFlagBits
-- | VkImageCreateFlags - Bitmask of VkImageCreateFlagBits
--
-- = Description
--
-- @VkImageCreateFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkImageCreateFlagBits'.
--
-- = See Also
--
-- 'VkImageCreateFlagBits',
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties'
type VkImageCreateFlags = VkImageCreateFlagBits
-- | VkFormatFeatureFlags - Bitmask of VkFormatFeatureFlagBits
--
-- = Description
--
-- @VkFormatFeatureFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkFormatFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'VkFormatFeatureFlagBits', 'VkFormatProperties'
type VkFormatFeatureFlags = VkFormatFeatureFlagBits
-- | VkSampleCountFlags - Bitmask of VkSampleCountFlagBits
--
-- = Description
--
-- @VkSampleCountFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkSampleCountFlagBits'.
--
-- = See Also
--
-- 'VkImageFormatProperties', 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT',
-- 'VkSampleCountFlagBits'
type VkSampleCountFlags = VkSampleCountFlagBits
-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferCopy',
-- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'VkImageFormatProperties',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo', 'VkMemoryHeap',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.Core10.Image.VkSubresourceLayout',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults',
-- 'Graphics.Vulkan.Core10.Memory.vkMapMemory'
type VkDeviceSize = Word64
