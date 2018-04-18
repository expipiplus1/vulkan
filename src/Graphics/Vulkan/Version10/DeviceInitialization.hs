{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version10.DeviceInitialization
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
  ( Word64
  , Word32
  , Word8
  )
import Foreign.C.Types
  ( CFloat(..)
  , CSize(..)
  , CChar(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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


import Graphics.Vulkan.Version10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFormat(..)
  , VkResult(..)
  , VkFlags
  )


-- ** VkImageTiling

-- | 
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

-- | 
pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling
pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

-- | 
pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling
pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1
-- ** VkImageType

-- | 
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

-- | 
pattern VK_IMAGE_TYPE_1D :: VkImageType
pattern VK_IMAGE_TYPE_1D = VkImageType 0

-- | 
pattern VK_IMAGE_TYPE_2D :: VkImageType
pattern VK_IMAGE_TYPE_2D = VkImageType 1

-- | 
pattern VK_IMAGE_TYPE_3D :: VkImageType
pattern VK_IMAGE_TYPE_3D = VkImageType 2
-- ** VkPhysicalDeviceType

-- | 
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

-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VkPhysicalDeviceType 1

-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VkPhysicalDeviceType 2

-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VkPhysicalDeviceType 3

-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_CPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4
-- ** VkSystemAllocationScope

-- | 
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

-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = VkSystemAllocationScope 0

-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = VkSystemAllocationScope 1

-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = VkSystemAllocationScope 2

-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = VkSystemAllocationScope 3

-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = VkSystemAllocationScope 4
-- ** VkInternalAllocationType

-- | 
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

-- | 
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE :: VkInternalAllocationType
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0
-- ** VkInstanceCreateFlags

-- | 
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

-- | 
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

-- | Queue supports graphics operations
pattern VK_QUEUE_GRAPHICS_BIT :: VkQueueFlagBits
pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 0x00000001

-- | Queue supports compute operations
pattern VK_QUEUE_COMPUTE_BIT :: VkQueueFlagBits
pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 0x00000002

-- | Queue supports transfer operations
pattern VK_QUEUE_TRANSFER_BIT :: VkQueueFlagBits
pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 0x00000004

-- | Queue supports sparse resource memory management operations
pattern VK_QUEUE_SPARSE_BINDING_BIT :: VkQueueFlagBits
pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 0x00000008
-- ** VkMemoryPropertyFlagBits

-- | 
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

-- | If otherwise stated, then allocate memory on device
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 0x00000001

-- | Memory is mappable by host
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 0x00000002

-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 0x00000004

-- | Memory will be cached by the host
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 0x00000008

-- | Memory may be allocated by the driver when it is required
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 0x00000010
-- ** VkMemoryHeapFlagBits

-- | 
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

-- | If set, heap represents device memory
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 0x00000001
-- ** VkImageUsageFlagBits

-- | 
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

-- | Can be used as a source of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 0x00000001

-- | Can be used as a destination of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 0x00000002

-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 0x00000004

-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 0x00000008

-- | Can be used as framebuffer color attachment
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000010

-- | Can be used as framebuffer depth/stencil attachment
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000020

-- | Image data not needed outside of rendering
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000040

-- | Can be used as framebuffer input attachment
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000080
-- ** VkImageCreateFlagBits

-- | 
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

-- | Image should support sparse backing
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlagBits 0x00000001

-- | Image should support sparse backing with partial residency
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlagBits 0x00000002

-- | Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlagBits 0x00000004

-- | Allows image views to have different format than the base image
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlagBits 0x00000008

-- | Allows creating image views with cube type from the created image
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000010
-- ** VkFormatFeatureFlagBits

-- | 
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

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VkFormatFeatureFlagBits 0x00000001

-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = VkFormatFeatureFlagBits 0x00000002

-- | Format supports atomic operations in case it is used for storage images
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VkFormatFeatureFlagBits 0x00000004

-- | Format can be used for uniform texel buffers (TBOs)
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000008

-- | Format can be used for storage texel buffers (IBOs)
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000010

-- | Format supports atomic operations in case it is used for storage texel buffers
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VkFormatFeatureFlagBits 0x00000020

-- | Format can be used for vertex buffers (VBOs)
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000040

-- | Format can be used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x00000080

-- | Format supports blending in case it is used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VkFormatFeatureFlagBits 0x00000100

-- | Format can be used for depth/stencil attachment images
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x00000200

-- | Format can be used as the source image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = VkFormatFeatureFlagBits 0x00000400

-- | Format can be used as the destination image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = VkFormatFeatureFlagBits 0x00000800

-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VkFormatFeatureFlagBits 0x00001000
-- ** VkSampleCountFlagBits

-- | 
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

-- | Sample count 1 supported
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x00000001

-- | Sample count 2 supported
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x00000002

-- | Sample count 4 supported
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x00000004

-- | Sample count 8 supported
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x00000008

-- | Sample count 16 supported
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x00000010

-- | Sample count 32 supported
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x00000020

-- | Sample count 64 supported
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x00000040
type VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE :: Integral a => a
pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
type VK_UUID_SIZE = 16
pattern VK_UUID_SIZE :: Integral a => a
pattern VK_UUID_SIZE = 16
type VK_MAX_MEMORY_TYPES = 32
pattern VK_MAX_MEMORY_TYPES :: Integral a => a
pattern VK_MAX_MEMORY_TYPES = 32
type VK_MAX_MEMORY_HEAPS = 16
pattern VK_MAX_MEMORY_HEAPS :: Integral a => a
pattern VK_MAX_MEMORY_HEAPS = 16
-- |
type PFN_vkInternalAllocationNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())
-- |
type PFN_vkInternalFreeNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())
-- |
type PFN_vkReallocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))
-- |
type PFN_vkAllocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))
-- |
type PFN_vkFreeFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ())
-- |
type PFN_vkVoidFunction = Ptr (() -> IO ())
-- |
data VkInstance_T
type VkInstance = Ptr VkInstance_T
-- |
data VkPhysicalDevice_T
type VkPhysicalDevice = Ptr VkPhysicalDevice_T
-- |
data VkDevice_T
type VkDevice = Ptr VkDevice_T
-- | 
foreign import ccall "vkCreateInstance" vkCreateInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
-- | 
foreign import ccall "vkDestroyInstance" vkDestroyInstance :: ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
-- | 
foreign import ccall "vkGetDeviceProcAddr" vkGetDeviceProcAddr :: ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- | 
foreign import ccall "vkGetInstanceProcAddr" vkGetInstanceProcAddr :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- | 
foreign import ccall "vkGetPhysicalDeviceProperties" vkGetPhysicalDeviceProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
-- | TODO: Struct comments
data VkExtent3D = VkExtent3D
  { vkWidth :: Word32
  , vkHeight :: Word32
  , vkDepth :: Word32
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
-- | TODO: Struct comments
data VkPhysicalDeviceProperties = VkPhysicalDeviceProperties
  { vkApiVersion :: Word32
  , vkDriverVersion :: Word32
  , vkVendorID :: Word32
  , vkDeviceID :: Word32
  , vkDeviceType :: VkPhysicalDeviceType
  , vkDeviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar
  , vkPipelineCacheUUID :: Vector VK_UUID_SIZE Word8
  , vkLimits :: VkPhysicalDeviceLimits
  , vkSparseProperties :: VkPhysicalDeviceSparseProperties
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
-- | TODO: Struct comments
data VkApplicationInfo = VkApplicationInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkApplicationName :: Ptr CChar
  , vkApplicationVersion :: Word32
  , vkEngineName :: Ptr CChar
  , vkEngineVersion :: Word32
  , vkApiVersion :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 16) (vkApplicationName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 24) (vkApplicationVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 32) (vkEngineName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 40) (vkEngineVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 44) (vkApiVersion (poked :: VkApplicationInfo))
-- | TODO: Struct comments
data VkAllocationCallbacks = VkAllocationCallbacks
  { vkUserData :: Ptr ()
  , vkPfnAllocation :: PFN_vkAllocationFunction
  , vkPfnReallocation :: PFN_vkReallocationFunction
  , vkPfnFree :: PFN_vkFreeFunction
  , vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , vkPfnInternalFree :: PFN_vkInternalFreeNotification
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkUserData (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 8) (vkPfnAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 16) (vkPfnReallocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 24) (vkPfnFree (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 32) (vkPfnInternalAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 40) (vkPfnInternalFree (poked :: VkAllocationCallbacks))
-- | TODO: Struct comments
data VkInstanceCreateInfo = VkInstanceCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkInstanceCreateFlags
  , vkApplicationInfo :: Ptr VkApplicationInfo
  , vkEnabledLayerCount :: Word32
  , vkEnabledLayerNames :: Ptr (Ptr CChar)
  , vkEnabledExtensionCount :: Word32
  , vkEnabledExtensionNames :: Ptr (Ptr CChar)
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkApplicationInfo (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkEnabledLayerNames (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkEnabledExtensionNames (poked :: VkInstanceCreateInfo))
-- | TODO: Struct comments
data VkQueueFamilyProperties = VkQueueFamilyProperties
  { vkQueueFlags :: VkQueueFlags
  , vkQueueCount :: Word32
  , vkTimestampValidBits :: Word32
  , vkMinImageTransferGranularity :: VkExtent3D
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
-- | TODO: Struct comments
data VkPhysicalDeviceMemoryProperties = VkPhysicalDeviceMemoryProperties
  { vkMemoryTypeCount :: Word32
  , vkMemoryTypes :: Vector VK_MAX_MEMORY_TYPES VkMemoryType
  , vkMemoryHeapCount :: Word32
  , vkMemoryHeaps :: Vector VK_MAX_MEMORY_HEAPS VkMemoryHeap
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
-- | TODO: Struct comments
data VkMemoryType = VkMemoryType
  { vkPropertyFlags :: VkMemoryPropertyFlags
  , vkHeapIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkMemoryType <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPropertyFlags (poked :: VkMemoryType))
                *> poke (ptr `plusPtr` 4) (vkHeapIndex (poked :: VkMemoryType))
-- | TODO: Struct comments
data VkMemoryHeap = VkMemoryHeap
  { vkSize :: VkDeviceSize
  , vkFlags :: VkMemoryHeapFlags
  }
  deriving (Eq, Show)

instance Storable VkMemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkMemoryHeap <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryHeap))
                *> poke (ptr `plusPtr` 8) (vkFlags (poked :: VkMemoryHeap))
-- | TODO: Struct comments
data VkFormatProperties = VkFormatProperties
  { vkLinearTilingFeatures :: VkFormatFeatureFlags
  , vkOptimalTilingFeatures :: VkFormatFeatureFlags
  , vkBufferFeatures :: VkFormatFeatureFlags
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
-- | TODO: Struct comments
data VkImageFormatProperties = VkImageFormatProperties
  { vkMaxExtent :: VkExtent3D
  , vkMaxMipLevels :: Word32
  , vkMaxArrayLayers :: Word32
  , vkSampleCounts :: VkSampleCountFlags
  , vkMaxResourceSize :: VkDeviceSize
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
-- | TODO: Struct comments
data VkPhysicalDeviceFeatures = VkPhysicalDeviceFeatures
  { vkRobustBufferAccess :: VkBool32
  , vkFullDrawIndexUint32 :: VkBool32
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
  , vkTextureCompressionETC2 :: VkBool32
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
  , vkShaderFloat64 :: VkBool32
  , vkShaderInt64 :: VkBool32
  , vkShaderInt16 :: VkBool32
  , vkShaderResourceResidency :: VkBool32
  , vkShaderResourceMinLod :: VkBool32
  , vkSparseBinding :: VkBool32
  , vkSparseResidencyBuffer :: VkBool32
  , vkSparseResidencyImage2D :: VkBool32
  , vkSparseResidencyImage3D :: VkBool32
  , vkSparseResidency2Samples :: VkBool32
  , vkSparseResidency4Samples :: VkBool32
  , vkSparseResidency8Samples :: VkBool32
  , vkSparseResidency16Samples :: VkBool32
  , vkSparseResidencyAliased :: VkBool32
  , vkVariableMultisampleRate :: VkBool32
  , vkInheritedQueries :: VkBool32
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
-- | TODO: Struct comments
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties
  { vkResidencyStandard2DBlockShape :: VkBool32
  , vkResidencyStandard2DMultisampleBlockShape :: VkBool32
  , vkResidencyStandard3DBlockShape :: VkBool32
  , vkResidencyAlignedMipSize :: VkBool32
  , vkResidencyNonResidentStrict :: VkBool32
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
-- | TODO: Struct comments
data VkPhysicalDeviceLimits = VkPhysicalDeviceLimits
  { vkMaxImageDimension1D :: Word32
  , vkMaxImageDimension2D :: Word32
  , vkMaxImageDimension3D :: Word32
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
  , vkMaxComputeWorkGroupCount :: Vector 3 Word32
  , vkMaxComputeWorkGroupInvocations :: Word32
  , vkMaxComputeWorkGroupSize :: Vector 3 Word32
  , vkSubPixelPrecisionBits :: Word32
  , vkSubTexelPrecisionBits :: Word32
  , vkMipmapPrecisionBits :: Word32
  , vkMaxDrawIndexedIndexValue :: Word32
  , vkMaxDrawIndirectCount :: Word32
  , vkMaxSamplerLodBias :: CFloat
  , vkMaxSamplerAnisotropy :: CFloat
  , vkMaxViewports :: Word32
  , vkMaxViewportDimensions :: Vector 2 Word32
  , vkViewportBoundsRange :: Vector 2 CFloat
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
  , vkPointSizeRange :: Vector 2 CFloat
  , vkLineWidthRange :: Vector 2 CFloat
  , vkPointSizeGranularity :: CFloat
  , vkLineWidthGranularity :: CFloat
  , vkStrictLines :: VkBool32
  , vkStandardSampleLocations :: VkBool32
  , vkOptimalBufferCopyOffsetAlignment :: VkDeviceSize
  , vkOptimalBufferCopyRowPitchAlignment :: VkDeviceSize
  , vkNonCoherentAtomSize :: VkDeviceSize
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
type VkQueueFlags = VkQueueFlagBits
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits
type VkMemoryHeapFlags = VkMemoryHeapFlagBits
type VkImageUsageFlags = VkImageUsageFlagBits
type VkImageCreateFlags = VkImageCreateFlagBits
type VkFormatFeatureFlags = VkFormatFeatureFlagBits
type VkSampleCountFlags = VkSampleCountFlagBits
-- |
type VkDeviceSize = Word64
