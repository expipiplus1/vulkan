{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core10.DeviceInitialization
  ( PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VK_MAX_MEMORY_HEAPS
  , pattern VK_MAX_MEMORY_HEAPS
  , VK_MAX_MEMORY_TYPES
  , pattern VK_MAX_MEMORY_TYPES
  , VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , VK_UUID_SIZE
  , pattern VK_UUID_SIZE
  , VkAllocationCallbacks(..)
  , VkApplicationInfo(..)
  , VkDevice
  , VkDeviceSize
  , VkExtent3D(..)
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
  , VkFormatFeatureFlags
  , VkFormatProperties(..)
  , VkImageCreateFlagBits(..)
  , pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , VkImageCreateFlags
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , pattern VK_IMAGE_TILING_OPTIMAL
  , pattern VK_IMAGE_TILING_LINEAR
  , VkImageType(..)
  , pattern VK_IMAGE_TYPE_1D
  , pattern VK_IMAGE_TYPE_2D
  , pattern VK_IMAGE_TYPE_3D
  , VkImageUsageFlagBits(..)
  , pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_USAGE_SAMPLED_BIT
  , pattern VK_IMAGE_USAGE_STORAGE_BIT
  , pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , VkImageUsageFlags
  , VkInstance
  , VkInstanceCreateFlags(..)
  , VkInstanceCreateInfo(..)
  , VkInternalAllocationType(..)
  , pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
  , VkMemoryHeap(..)
  , VkMemoryHeapFlagBits(..)
  , pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
  , VkMemoryHeapFlags
  , VkMemoryPropertyFlagBits(..)
  , pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , VkMemoryPropertyFlags
  , VkMemoryType(..)
  , VkPhysicalDevice
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkPhysicalDeviceType(..)
  , pattern VK_PHYSICAL_DEVICE_TYPE_OTHER
  , pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_CPU
  , VkQueueFamilyProperties(..)
  , VkQueueFlagBits(..)
  , pattern VK_QUEUE_GRAPHICS_BIT
  , pattern VK_QUEUE_COMPUTE_BIT
  , pattern VK_QUEUE_TRANSFER_BIT
  , pattern VK_QUEUE_SPARSE_BINDING_BIT
  , VkQueueFlags
  , VkSampleCountFlagBits(..)
  , pattern VK_SAMPLE_COUNT_1_BIT
  , pattern VK_SAMPLE_COUNT_2_BIT
  , pattern VK_SAMPLE_COUNT_4_BIT
  , pattern VK_SAMPLE_COUNT_8_BIT
  , pattern VK_SAMPLE_COUNT_16_BIT
  , pattern VK_SAMPLE_COUNT_32_BIT
  , pattern VK_SAMPLE_COUNT_64_BIT
  , VkSampleCountFlags
  , VkSystemAllocationScope(..)
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
  , FN_vkCreateInstance
  , PFN_vkCreateInstance
  , vkCreateInstance
  , FN_vkDestroyInstance
  , PFN_vkDestroyInstance
  , vkDestroyInstance
  , FN_vkEnumeratePhysicalDevices
  , PFN_vkEnumeratePhysicalDevices
  , vkEnumeratePhysicalDevices
  , FN_vkGetDeviceProcAddr
  , PFN_vkGetDeviceProcAddr
  , vkGetDeviceProcAddr
  , vkGetInstanceProcAddr'
  , FN_vkGetInstanceProcAddr
  , PFN_vkGetInstanceProcAddr
  , vkGetInstanceProcAddr
  , FN_vkGetPhysicalDeviceFeatures
  , PFN_vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceFeatures
  , FN_vkGetPhysicalDeviceFormatProperties
  , PFN_vkGetPhysicalDeviceFormatProperties
  , vkGetPhysicalDeviceFormatProperties
  , FN_vkGetPhysicalDeviceImageFormatProperties
  , PFN_vkGetPhysicalDeviceImageFormatProperties
  , vkGetPhysicalDeviceImageFormatProperties
  , FN_vkGetPhysicalDeviceMemoryProperties
  , PFN_vkGetPhysicalDeviceMemoryProperties
  , vkGetPhysicalDeviceMemoryProperties
  , FN_vkGetPhysicalDeviceProperties
  , PFN_vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceProperties
  , FN_vkGetPhysicalDeviceQueueFamilyProperties
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties
  , vkGetPhysicalDeviceQueueFamilyProperties
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
  ( FunPtr
  , Ptr
  , castPtrToFunPtr
  , nullPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import qualified GHC.Ptr
  ( Ptr(Ptr)
  )
import GHC.Read
  ( choose
  , expectP
  )
import System.IO.Unsafe
  ( unsafeDupablePerformIO
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "PFN_vkAllocationFunction"
type PFN_vkAllocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))

-- No documentation found for TopLevel "PFN_vkFreeFunction"
type PFN_vkFreeFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ())

-- No documentation found for TopLevel "PFN_vkInternalAllocationNotification"
type PFN_vkInternalAllocationNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())

-- No documentation found for TopLevel "PFN_vkInternalFreeNotification"
type PFN_vkInternalFreeNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())

-- No documentation found for TopLevel "PFN_vkReallocationFunction"
type PFN_vkReallocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))

-- No documentation found for TopLevel "PFN_vkVoidFunction"
type PFN_vkVoidFunction = Ptr (() -> IO ())

-- No documentation found for TopLevel "VK_MAX_MEMORY_HEAPS"
type VK_MAX_MEMORY_HEAPS = 16
-- No documentation found for Nested "Integral a => a" "VK_MAX_MEMORY_HEAPS"
pattern VK_MAX_MEMORY_HEAPS :: Integral a => a
pattern VK_MAX_MEMORY_HEAPS = 16

-- No documentation found for TopLevel "VK_MAX_MEMORY_TYPES"
type VK_MAX_MEMORY_TYPES = 32
-- No documentation found for Nested "Integral a => a" "VK_MAX_MEMORY_TYPES"
pattern VK_MAX_MEMORY_TYPES :: Integral a => a
pattern VK_MAX_MEMORY_TYPES = 32

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

-- No documentation found for TopLevel "VkAllocationCallbacks"
data VkAllocationCallbacks = VkAllocationCallbacks
  { -- No documentation found for Nested "VkAllocationCallbacks" "pUserData"
  vkPUserData :: Ptr ()
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnAllocation"
  vkPfnAllocation :: PFN_vkAllocationFunction
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnReallocation"
  vkPfnReallocation :: PFN_vkReallocationFunction
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnFree"
  vkPfnFree :: PFN_vkFreeFunction
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnInternalAllocation"
  vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- No documentation found for Nested "VkAllocationCallbacks" "pfnInternalFree"
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

instance Zero VkAllocationCallbacks where
  zero = VkAllocationCallbacks zero
                               zero
                               zero
                               zero
                               zero
                               zero

-- No documentation found for TopLevel "VkApplicationInfo"
data VkApplicationInfo = VkApplicationInfo
  { -- No documentation found for Nested "VkApplicationInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkApplicationInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkApplicationInfo" "pApplicationName"
  vkPApplicationName :: Ptr CChar
  , -- No documentation found for Nested "VkApplicationInfo" "applicationVersion"
  vkApplicationVersion :: Word32
  , -- No documentation found for Nested "VkApplicationInfo" "pEngineName"
  vkPEngineName :: Ptr CChar
  , -- No documentation found for Nested "VkApplicationInfo" "engineVersion"
  vkEngineVersion :: Word32
  , -- No documentation found for Nested "VkApplicationInfo" "apiVersion"
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

instance Zero VkApplicationInfo where
  zero = VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero

-- | Dummy data to tag the 'Ptr' with
data VkDevice_T
-- No documentation found for TopLevel "VkDevice"
type VkDevice = Ptr VkDevice_T

-- No documentation found for TopLevel "VkDeviceSize"
type VkDeviceSize = Word64

-- No documentation found for TopLevel "VkExtent3D"
data VkExtent3D = VkExtent3D
  { -- No documentation found for Nested "VkExtent3D" "width"
  vkWidth :: Word32
  , -- No documentation found for Nested "VkExtent3D" "height"
  vkHeight :: Word32
  , -- No documentation found for Nested "VkExtent3D" "depth"
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

instance Zero VkExtent3D where
  zero = VkExtent3D zero
                    zero
                    zero

-- ** VkFormatFeatureFlagBits

-- No documentation found for TopLevel "VkFormatFeatureFlagBits"
newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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
  showsPrec _ (VkFormatFeatureFlagBits 0x08000000) = showString "VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x10000000) = showString "VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x02000000) = showString "VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x04000000) = showString "VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x00010000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
  showsPrec _ (VkFormatFeatureFlagBits 0x01000000) = showString "VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
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
                             , ("VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x08000000))
                             , ("VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x10000000))
                             , ("VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x02000000))
                             , ("VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x04000000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT",                                         pure (VkFormatFeatureFlagBits 0x00010000))
                             , ("VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT",                                                pure (VkFormatFeatureFlagBits 0x01000000))
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

-- No documentation found for TopLevel "VkFormatFeatureFlags"
type VkFormatFeatureFlags = VkFormatFeatureFlagBits

-- No documentation found for TopLevel "VkFormatProperties"
data VkFormatProperties = VkFormatProperties
  { -- No documentation found for Nested "VkFormatProperties" "linearTilingFeatures"
  vkLinearTilingFeatures :: VkFormatFeatureFlags
  , -- No documentation found for Nested "VkFormatProperties" "optimalTilingFeatures"
  vkOptimalTilingFeatures :: VkFormatFeatureFlags
  , -- No documentation found for Nested "VkFormatProperties" "bufferFeatures"
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

instance Zero VkFormatProperties where
  zero = VkFormatProperties zero
                            zero
                            zero

-- ** VkImageCreateFlagBits

-- No documentation found for TopLevel "VkImageCreateFlagBits"
newtype VkImageCreateFlagBits = VkImageCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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
  showsPrec _ (VkImageCreateFlagBits 0x00002000) = showString "VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
  showsPrec _ (VkImageCreateFlagBits 0x00001000) = showString "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
  showsPrec _ (VkImageCreateFlagBits 0x00004000) = showString "VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
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
                             , ("VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV",                     pure (VkImageCreateFlagBits 0x00002000))
                             , ("VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT", pure (VkImageCreateFlagBits 0x00001000))
                             , ("VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT",                        pure (VkImageCreateFlagBits 0x00004000))
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

-- No documentation found for TopLevel "VkImageCreateFlags"
type VkImageCreateFlags = VkImageCreateFlagBits

-- No documentation found for TopLevel "VkImageFormatProperties"
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

instance Zero VkImageFormatProperties where
  zero = VkImageFormatProperties zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkImageTiling

-- No documentation found for TopLevel "VkImageTiling"
newtype VkImageTiling = VkImageTiling Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkImageTiling where
  showsPrec _ VK_IMAGE_TILING_OPTIMAL = showString "VK_IMAGE_TILING_OPTIMAL"
  showsPrec _ VK_IMAGE_TILING_LINEAR = showString "VK_IMAGE_TILING_LINEAR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageTiling 1000158000) = showString "VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT"
  showsPrec p (VkImageTiling x) = showParen (p >= 11) (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
  readPrec = parens ( choose [ ("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL)
                             , ("VK_IMAGE_TILING_LINEAR",  pure VK_IMAGE_TILING_LINEAR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT", pure (VkImageTiling 1000158000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageTiling")
                        v <- step readPrec
                        pure (VkImageTiling v)
                        )
                    )

-- No documentation found for Nested "VkImageTiling" "VK_IMAGE_TILING_OPTIMAL"
pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling
pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

-- No documentation found for Nested "VkImageTiling" "VK_IMAGE_TILING_LINEAR"
pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling
pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- ** VkImageType

-- No documentation found for TopLevel "VkImageType"
newtype VkImageType = VkImageType Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkImageType" "VK_IMAGE_TYPE_1D"
pattern VK_IMAGE_TYPE_1D :: VkImageType
pattern VK_IMAGE_TYPE_1D = VkImageType 0

-- No documentation found for Nested "VkImageType" "VK_IMAGE_TYPE_2D"
pattern VK_IMAGE_TYPE_2D :: VkImageType
pattern VK_IMAGE_TYPE_2D = VkImageType 1

-- No documentation found for Nested "VkImageType" "VK_IMAGE_TYPE_3D"
pattern VK_IMAGE_TYPE_3D :: VkImageType
pattern VK_IMAGE_TYPE_3D = VkImageType 2

-- ** VkImageUsageFlagBits

-- No documentation found for TopLevel "VkImageUsageFlagBits"
newtype VkImageUsageFlagBits = VkImageUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkImageUsageFlagBits where
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
  showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT = showString "VK_IMAGE_USAGE_STORAGE_BIT"
  showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageUsageFlagBits 0x00002000) = showString "VK_IMAGE_USAGE_RESERVED_13_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00004000) = showString "VK_IMAGE_USAGE_RESERVED_14_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00008000) = showString "VK_IMAGE_USAGE_RESERVED_15_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00000400) = showString "VK_IMAGE_USAGE_RESERVED_10_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00000800) = showString "VK_IMAGE_USAGE_RESERVED_11_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00001000) = showString "VK_IMAGE_USAGE_RESERVED_12_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00000100) = showString "VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
  showsPrec _ (VkImageUsageFlagBits 0x00000200) = showString "VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
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
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_USAGE_RESERVED_13_BIT_KHR",          pure (VkImageUsageFlagBits 0x00002000))
                             , ("VK_IMAGE_USAGE_RESERVED_14_BIT_KHR",          pure (VkImageUsageFlagBits 0x00004000))
                             , ("VK_IMAGE_USAGE_RESERVED_15_BIT_KHR",          pure (VkImageUsageFlagBits 0x00008000))
                             , ("VK_IMAGE_USAGE_RESERVED_10_BIT_KHR",          pure (VkImageUsageFlagBits 0x00000400))
                             , ("VK_IMAGE_USAGE_RESERVED_11_BIT_KHR",          pure (VkImageUsageFlagBits 0x00000800))
                             , ("VK_IMAGE_USAGE_RESERVED_12_BIT_KHR",          pure (VkImageUsageFlagBits 0x00001000))
                             , ("VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV",    pure (VkImageUsageFlagBits 0x00000100))
                             , ("VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT", pure (VkImageUsageFlagBits 0x00000200))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageUsageFlagBits")
                        v <- step readPrec
                        pure (VkImageUsageFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 0x00000001

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 0x00000002

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SAMPLED_BIT"
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 0x00000004

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_STORAGE_BIT"
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 0x00000008

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000010

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000020

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000040

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000080

-- No documentation found for TopLevel "VkImageUsageFlags"
type VkImageUsageFlags = VkImageUsageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkInstance_T
-- No documentation found for TopLevel "VkInstance"
type VkInstance = Ptr VkInstance_T

-- ** VkInstanceCreateFlags

-- No documentation found for TopLevel "VkInstanceCreateFlags"
newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkInstanceCreateInfo"
data VkInstanceCreateInfo = VkInstanceCreateInfo
  { -- No documentation found for Nested "VkInstanceCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkInstanceCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkInstanceCreateInfo" "flags"
  vkFlags :: VkInstanceCreateFlags
  , -- No documentation found for Nested "VkInstanceCreateInfo" "pApplicationInfo"
  vkPApplicationInfo :: Ptr VkApplicationInfo
  , -- No documentation found for Nested "VkInstanceCreateInfo" "enabledLayerCount"
  vkEnabledLayerCount :: Word32
  , -- No documentation found for Nested "VkInstanceCreateInfo" "ppEnabledLayerNames"
  vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , -- No documentation found for Nested "VkInstanceCreateInfo" "enabledExtensionCount"
  vkEnabledExtensionCount :: Word32
  , -- No documentation found for Nested "VkInstanceCreateInfo" "ppEnabledExtensionNames"
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

instance Zero VkInstanceCreateInfo where
  zero = VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero

-- ** VkInternalAllocationType

-- No documentation found for TopLevel "VkInternalAllocationType"
newtype VkInternalAllocationType = VkInternalAllocationType Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkInternalAllocationType" "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE :: VkInternalAllocationType
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0

-- No documentation found for TopLevel "VkMemoryHeap"
data VkMemoryHeap = VkMemoryHeap
  { -- No documentation found for Nested "VkMemoryHeap" "size"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkMemoryHeap" "flags"
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

instance Zero VkMemoryHeap where
  zero = VkMemoryHeap zero
                      zero

-- ** VkMemoryHeapFlagBits

-- No documentation found for TopLevel "VkMemoryHeapFlagBits"
newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkMemoryHeapFlagBits" "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 0x00000001

-- No documentation found for TopLevel "VkMemoryHeapFlags"
type VkMemoryHeapFlags = VkMemoryHeapFlagBits

-- ** VkMemoryPropertyFlagBits

-- No documentation found for TopLevel "VkMemoryPropertyFlagBits"
newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 0x00000001

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 0x00000002

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 0x00000004

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 0x00000008

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 0x00000010

-- No documentation found for TopLevel "VkMemoryPropertyFlags"
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits

-- No documentation found for TopLevel "VkMemoryType"
data VkMemoryType = VkMemoryType
  { -- No documentation found for Nested "VkMemoryType" "propertyFlags"
  vkPropertyFlags :: VkMemoryPropertyFlags
  , -- No documentation found for Nested "VkMemoryType" "heapIndex"
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

instance Zero VkMemoryType where
  zero = VkMemoryType zero
                      zero

-- | Dummy data to tag the 'Ptr' with
data VkPhysicalDevice_T
-- No documentation found for TopLevel "VkPhysicalDevice"
type VkPhysicalDevice = Ptr VkPhysicalDevice_T

-- No documentation found for TopLevel "VkPhysicalDeviceFeatures"
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

instance Zero VkPhysicalDeviceFeatures where
  zero = VkPhysicalDeviceFeatures zero
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
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero

-- No documentation found for TopLevel "VkPhysicalDeviceLimits"
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

instance Zero VkPhysicalDeviceLimits where
  zero = VkPhysicalDeviceLimits zero
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

-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties"
data VkPhysicalDeviceMemoryProperties = VkPhysicalDeviceMemoryProperties
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryTypeCount"
  vkMemoryTypeCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryTypes"
  vkMemoryTypes :: Vector VK_MAX_MEMORY_TYPES VkMemoryType
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryHeapCount"
  vkMemoryHeapCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryHeaps"
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

instance Zero VkPhysicalDeviceMemoryProperties where
  zero = VkPhysicalDeviceMemoryProperties zero
                                          zero
                                          zero
                                          zero

-- No documentation found for TopLevel "VkPhysicalDeviceProperties"
data VkPhysicalDeviceProperties = VkPhysicalDeviceProperties
  { -- No documentation found for Nested "VkPhysicalDeviceProperties" "apiVersion"
  vkApiVersion :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "driverVersion"
  vkDriverVersion :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "vendorID"
  vkVendorID :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "deviceID"
  vkDeviceID :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "deviceType"
  vkDeviceType :: VkPhysicalDeviceType
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "deviceName"
  vkDeviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "pipelineCacheUUID"
  vkPipelineCacheUUID :: Vector VK_UUID_SIZE Word8
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "limits"
  vkLimits :: VkPhysicalDeviceLimits
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "sparseProperties"
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

instance Zero VkPhysicalDeviceProperties where
  zero = VkPhysicalDeviceProperties zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- No documentation found for TopLevel "VkPhysicalDeviceSparseProperties"
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties
  { -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyStandard2DBlockShape"
  vkResidencyStandard2DBlockShape :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyStandard2DMultisampleBlockShape"
  vkResidencyStandard2DMultisampleBlockShape :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyStandard3DBlockShape"
  vkResidencyStandard3DBlockShape :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyAlignedMipSize"
  vkResidencyAlignedMipSize :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyNonResidentStrict"
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

instance Zero VkPhysicalDeviceSparseProperties where
  zero = VkPhysicalDeviceSparseProperties zero
                                          zero
                                          zero
                                          zero
                                          zero

-- ** VkPhysicalDeviceType

-- No documentation found for TopLevel "VkPhysicalDeviceType"
newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for TopLevel "VkQueueFamilyProperties"
data VkQueueFamilyProperties = VkQueueFamilyProperties
  { -- No documentation found for Nested "VkQueueFamilyProperties" "queueFlags"
  vkQueueFlags :: VkQueueFlags
  , -- No documentation found for Nested "VkQueueFamilyProperties" "queueCount"
  vkQueueCount :: Word32
  , -- No documentation found for Nested "VkQueueFamilyProperties" "timestampValidBits"
  vkTimestampValidBits :: Word32
  , -- No documentation found for Nested "VkQueueFamilyProperties" "minImageTransferGranularity"
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

instance Zero VkQueueFamilyProperties where
  zero = VkQueueFamilyProperties zero
                                 zero
                                 zero
                                 zero

-- ** VkQueueFlagBits

-- No documentation found for TopLevel "VkQueueFlagBits"
newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkQueueFlagBits where
  showsPrec _ VK_QUEUE_GRAPHICS_BIT = showString "VK_QUEUE_GRAPHICS_BIT"
  showsPrec _ VK_QUEUE_COMPUTE_BIT = showString "VK_QUEUE_COMPUTE_BIT"
  showsPrec _ VK_QUEUE_TRANSFER_BIT = showString "VK_QUEUE_TRANSFER_BIT"
  showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT = showString "VK_QUEUE_SPARSE_BINDING_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkQueueFlagBits 0x00000010) = showString "VK_QUEUE_PROTECTED_BIT"
  showsPrec _ (VkQueueFlagBits 0x00000040) = showString "VK_QUEUE_RESERVED_6_BIT_KHR"
  showsPrec _ (VkQueueFlagBits 0x00000020) = showString "VK_QUEUE_RESERVED_5_BIT_KHR"
  showsPrec p (VkQueueFlagBits x) = showParen (p >= 11) (showString "VkQueueFlagBits " . showsPrec 11 x)

instance Read VkQueueFlagBits where
  readPrec = parens ( choose [ ("VK_QUEUE_GRAPHICS_BIT",       pure VK_QUEUE_GRAPHICS_BIT)
                             , ("VK_QUEUE_COMPUTE_BIT",        pure VK_QUEUE_COMPUTE_BIT)
                             , ("VK_QUEUE_TRANSFER_BIT",       pure VK_QUEUE_TRANSFER_BIT)
                             , ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_QUEUE_PROTECTED_BIT",      pure (VkQueueFlagBits 0x00000010))
                             , ("VK_QUEUE_RESERVED_6_BIT_KHR", pure (VkQueueFlagBits 0x00000040))
                             , ("VK_QUEUE_RESERVED_5_BIT_KHR", pure (VkQueueFlagBits 0x00000020))
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

-- No documentation found for TopLevel "VkQueueFlags"
type VkQueueFlags = VkQueueFlagBits

-- ** VkSampleCountFlagBits

-- No documentation found for TopLevel "VkSampleCountFlagBits"
newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_1_BIT"
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x00000001

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_2_BIT"
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x00000002

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_4_BIT"
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x00000004

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_8_BIT"
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x00000008

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_16_BIT"
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x00000010

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_32_BIT"
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x00000020

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_64_BIT"
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x00000040

-- No documentation found for TopLevel "VkSampleCountFlags"
type VkSampleCountFlags = VkSampleCountFlagBits

-- ** VkSystemAllocationScope

-- No documentation found for TopLevel "VkSystemAllocationScope"
newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for TopLevel "vkCreateInstance"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateInstance" vkCreateInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateInstance
  :: FunPtr (("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult) -> (("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult)

vkCreateInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
vkCreateInstance = mkVkCreateInstance procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkCreateInstance $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkCreateInstance\NUL"#)
#endif

type FN_vkCreateInstance = ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
type PFN_vkCreateInstance = FunPtr FN_vkCreateInstance

-- No documentation found for TopLevel "vkDestroyInstance"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyInstance" vkDestroyInstance :: ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyInstance :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyInstance deviceCmds = mkVkDestroyInstance (pVkDestroyInstance deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyInstance
  :: FunPtr (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyInstance = ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyInstance = FunPtr FN_vkDestroyInstance

-- No documentation found for TopLevel "vkEnumeratePhysicalDevices"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
#else
vkEnumeratePhysicalDevices :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
vkEnumeratePhysicalDevices deviceCmds = mkVkEnumeratePhysicalDevices (pVkEnumeratePhysicalDevices deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDevices
  :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult)
#endif

type FN_vkEnumeratePhysicalDevices = ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
type PFN_vkEnumeratePhysicalDevices = FunPtr FN_vkEnumeratePhysicalDevices

-- No documentation found for TopLevel "vkGetDeviceProcAddr"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceProcAddr" vkGetDeviceProcAddr :: ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
#else
vkGetDeviceProcAddr :: DeviceCmds -> ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
vkGetDeviceProcAddr deviceCmds = mkVkGetDeviceProcAddr (pVkGetDeviceProcAddr deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
  :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#endif

type FN_vkGetDeviceProcAddr = ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetDeviceProcAddr = FunPtr FN_vkGetDeviceProcAddr

-- No documentation found for TopLevel "vkGetInstanceProcAddr"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" vkGetInstanceProcAddr :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
#else
vkGetInstanceProcAddr :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
vkGetInstanceProcAddr deviceCmds = mkVkGetInstanceProcAddr (pVkGetInstanceProcAddr deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetInstanceProcAddr
  :: FunPtr (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#endif
-- | A version of 'vkGetInstanceProcAddr' which can be called with a
-- null pointer for the instance.
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" vkGetInstanceProcAddr' :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type FN_vkGetInstanceProcAddr = ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetInstanceProcAddr = FunPtr FN_vkGetInstanceProcAddr

-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
#else
vkGetPhysicalDeviceFeatures :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
vkGetPhysicalDeviceFeatures deviceCmds = mkVkGetPhysicalDeviceFeatures (pVkGetPhysicalDeviceFeatures deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ())
#endif

type FN_vkGetPhysicalDeviceFeatures = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures = FunPtr FN_vkGetPhysicalDeviceFeatures

-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
#else
vkGetPhysicalDeviceFormatProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
vkGetPhysicalDeviceFormatProperties deviceCmds = mkVkGetPhysicalDeviceFormatProperties (pVkGetPhysicalDeviceFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties = FunPtr FN_vkGetPhysicalDeviceFormatProperties

-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
#else
vkGetPhysicalDeviceImageFormatProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties deviceCmds = mkVkGetPhysicalDeviceImageFormatProperties (pVkGetPhysicalDeviceImageFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceImageFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties

-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
#else
vkGetPhysicalDeviceMemoryProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
vkGetPhysicalDeviceMemoryProperties deviceCmds = mkVkGetPhysicalDeviceMemoryProperties (pVkGetPhysicalDeviceMemoryProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceMemoryProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties = FunPtr FN_vkGetPhysicalDeviceMemoryProperties

-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceProperties" vkGetPhysicalDeviceProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
#else
vkGetPhysicalDeviceProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
vkGetPhysicalDeviceProperties deviceCmds = mkVkGetPhysicalDeviceProperties (pVkGetPhysicalDeviceProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
type PFN_vkGetPhysicalDeviceProperties = FunPtr FN_vkGetPhysicalDeviceProperties

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
#else
vkGetPhysicalDeviceQueueFamilyProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties deviceCmds = mkVkGetPhysicalDeviceQueueFamilyProperties (pVkGetPhysicalDeviceQueueFamilyProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceQueueFamilyProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties
