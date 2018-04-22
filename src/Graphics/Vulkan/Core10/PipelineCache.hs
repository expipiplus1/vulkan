{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags(..)
  , VkPipelineCache
  , vkCreatePipelineCache
  , vkDestroyPipelineCache
  , vkGetPipelineCacheData
  , vkMergePipelineCaches
  , VkPipelineCacheCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
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
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkPipelineCacheCreateFlags

-- | VkPipelineCacheCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkPipelineCacheCreateFlags@ is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineCacheCreateInfo'
newtype VkPipelineCacheCreateFlags = VkPipelineCacheCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineCacheCreateFlags where
  
  showsPrec p (VkPipelineCacheCreateFlags x) = showParen (p >= 11) (showString "VkPipelineCacheCreateFlags " . showsPrec 11 x)

instance Read VkPipelineCacheCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCacheCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineCacheCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkPipelineCache_T
-- | VkPipelineCache - Opaque handle to a pipeline cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'vkCreatePipelineCache', 'vkDestroyPipelineCache',
-- 'vkGetPipelineCacheData', 'vkMergePipelineCaches'
type VkPipelineCache = Ptr VkPipelineCache_T
-- | vkCreatePipelineCache - Creates a new pipeline cache
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the pipeline cache
--     object.
--
-- -   @pCreateInfo@ is a pointer to a @VkPipelineCacheCreateInfo@
--     structure that contains the initial parameters for the pipeline
--     cache object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pPipelineCache@ is a pointer to a @VkPipelineCache@ handle in which
--     the resulting pipeline cache object is returned.
--
-- = Description
--
-- __Note__
--
-- Applications /can/ track and manage the total host memory size of a
-- pipeline cache object using the @pAllocator@. Applications /can/ limit
-- the amount of data retrieved from a pipeline cache object in
-- @vkGetPipelineCacheData@. Implementations /should/ not internally limit
-- the total number of entries added to a pipeline cache object or the
-- total host memory consumed.
--
-- Once created, a pipeline cache /can/ be passed to the
-- @vkCreateGraphicsPipelines@ and @vkCreateComputePipelines@ commands. If
-- the pipeline cache passed into these commands is not
-- 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', the implementation
-- will query it for possible reuse opportunities and update it with new
-- content. The use of the pipeline cache object in these commands is
-- internally synchronized, and the same pipeline cache object /can/ be
-- used in multiple threads simultaneously.
--
-- __Note__
--
-- Implementations /should/ make every effort to limit any critical
-- sections to the actual accesses to the cache, which is expected to be
-- significantly shorter than the duration of the
-- @vkCreateGraphicsPipelines@ and @vkCreateComputePipelines@ commands.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkPipelineCacheCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pPipelineCache@ /must/ be a valid pointer to a @VkPipelineCache@
--     handle
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
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkPipelineCache', 'VkPipelineCacheCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreatePipelineCache" vkCreatePipelineCache :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
-- | vkDestroyPipelineCache - Destroy a pipeline cache object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the pipeline cache
--     object.
--
-- -   @pipelineCache@ is the handle of the pipeline cache to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @pipelineCache@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @pipelineCache@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @pipelineCache@
--     /must/ be a valid @VkPipelineCache@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipelineCache@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkPipelineCache'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipelineCache" vkDestroyPipelineCache :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkGetPipelineCacheData - Get the data store from a pipeline cache
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the pipeline cache.
--
-- -   @pipelineCache@ is the pipeline cache to retrieve data from.
--
-- -   @pDataSize@ is a pointer to a value related to the amount of data in
--     the pipeline cache, as described below.
--
-- -   @pData@ is either @NULL@ or a pointer to a buffer.
--
-- = Description
--
-- If @pData@ is @NULL@, then the maximum size of the data that /can/ be
-- retrieved from the pipeline cache, in bytes, is returned in @pDataSize@.
-- Otherwise, @pDataSize@ /must/ point to a variable set by the user to the
-- size of the buffer, in bytes, pointed to by @pData@, and on return the
-- variable is overwritten with the amount of data actually written to
-- @pData@.
--
-- If @pDataSize@ is less than the maximum size that /can/ be retrieved by
-- the pipeline cache, at most @pDataSize@ bytes will be written to
-- @pData@, and @vkGetPipelineCacheData@ will return @VK_INCOMPLETE@. Any
-- data written to @pData@ is valid and /can/ be provided as the
-- @pInitialData@ member of the @VkPipelineCacheCreateInfo@ structure
-- passed to @vkCreatePipelineCache@.
--
-- Two calls to @vkGetPipelineCacheData@ with the same parameters /must/
-- retrieve the same data unless a command that modifies the contents of
-- the cache is called between them.
--
-- Applications /can/ store the data retrieved from the pipeline cache, and
-- use these data, possibly in a future run of the application, to populate
-- new pipeline cache objects. The results of pipeline compiles, however,
-- /may/ depend on the vendor ID, device ID, driver version, and other
-- details of the device. To enable applications to detect when previously
-- retrieved data is incompatible with the device, the initial bytes
-- written to @pData@ /must/ be a header consisting of the following
-- members:
--
-- +--------+----------------+-----------------------------------------------------------------+
-- | Offset | Size           | Meaning                                                         |
-- +========+================+=================================================================+
-- | 0      | 4              | length in bytes of the entire pipeline cache header written as  |
-- |        |                | a stream of bytes, with the least significant byte first        |
-- +--------+----------------+-----------------------------------------------------------------+
-- | 4      | 4              | a                                                               |
-- |        |                | 'Graphics.Vulkan.Core10.Constants.VkPipelineCacheHeaderVersion' |
-- |        |                | value written as a stream of bytes, with the least significant  |
-- |        |                | byte first                                                      |
-- +--------+----------------+-----------------------------------------------------------------+
-- | 8      | 4              | a vendor ID equal to @VkPhysicalDeviceProperties@::@vendorID@   |
-- |        |                | written as a stream of bytes, with the least significant byte   |
-- |        |                | first                                                           |
-- +--------+----------------+-----------------------------------------------------------------+
-- | 12     | 4              | a device ID equal to @VkPhysicalDeviceProperties@::@deviceID@   |
-- |        |                | written as a stream of bytes, with the least significant byte   |
-- |        |                | first                                                           |
-- +--------+----------------+-----------------------------------------------------------------+
-- | 16     | @VK_UUID_SIZE@ | a pipeline cache ID equal to                                    |
-- |        |                | @VkPhysicalDeviceProperties@::@pipelineCacheUUID@               |
-- +--------+----------------+-----------------------------------------------------------------+
--
-- Layout for pipeline cache header version
-- @VK_PIPELINE_CACHE_HEADER_VERSION_ONE@
--
-- The first four bytes encode the length of the entire pipeline cache
-- header, in bytes. This value includes all fields in the header including
-- the pipeline cache version field and the size of the length field.
--
-- The next four bytes encode the pipeline cache version, as described for
-- 'Graphics.Vulkan.Core10.Constants.VkPipelineCacheHeaderVersion'. A
-- consumer of the pipeline cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pipelineCache@ /must/ be a valid @VkPipelineCache@ handle
--
-- -   @pDataSize@ /must/ be a valid pointer to a @size_t@ value
--
-- -   If the value referenced by @pDataSize@ is not @0@, and @pData@ is
--     not @NULL@, @pData@ /must/ be a valid pointer to an array of
--     @pDataSize@ bytes
--
-- -   @pipelineCache@ /must/ have been created, allocated, or retrieved
--     from @device@
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
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkPipelineCache'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPipelineCacheData" vkGetPipelineCacheData :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
-- | vkMergePipelineCaches - Combine the data stores of pipeline caches
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the pipeline cache objects.
--
-- -   @dstCache@ is the handle of the pipeline cache to merge results
--     into.
--
-- -   @srcCacheCount@ is the length of the @pSrcCaches@ array.
--
-- -   @pSrcCaches@ is an array of pipeline cache handles, which will be
--     merged into @dstCache@. The previous contents of @dstCache@ are
--     included after the merge.
--
-- = Description
--
-- __Note__
--
-- The details of the merge operation are implementation dependent, but
-- implementations /should/ merge the contents of the specified pipelines
-- and prune duplicate entries.
--
-- == Valid Usage
--
-- -   @dstCache@ /must/ not appear in the list of source caches
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @dstCache@ /must/ be a valid @VkPipelineCache@ handle
--
-- -   @pSrcCaches@ /must/ be a valid pointer to an array of
--     @srcCacheCount@ valid @VkPipelineCache@ handles
--
-- -   @srcCacheCount@ /must/ be greater than @0@
--
-- -   @dstCache@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- -   Each element of @pSrcCaches@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @dstCache@ /must/ be externally synchronized
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
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkPipelineCache'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMergePipelineCaches" vkMergePipelineCaches :: ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
-- | VkPipelineCacheCreateInfo - Structure specifying parameters of a newly
-- created pipeline cache
--
-- == Valid Usage
--
-- -   If @initialDataSize@ is not @0@, it /must/ be equal to the size of
--     @pInitialData@, as returned by @vkGetPipelineCacheData@ when
--     @pInitialData@ was originally retrieved
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ have been
--     retrieved from a previous call to @vkGetPipelineCacheData@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ be a valid
--     pointer to an array of @initialDataSize@ bytes
--
-- = See Also
--
-- 'VkPipelineCacheCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreatePipelineCache'
data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineCacheCreateFlags
  , -- | @initialDataSize@ is the number of bytes in @pInitialData@. If
  -- @initialDataSize@ is zero, the pipeline cache will initially be empty.
  vkInitialDataSize :: CSize
  , -- | @pInitialData@ is a pointer to previously retrieved pipeline cache data.
  -- If the pipeline cache data is incompatible (as defined below) with the
  -- device, the pipeline cache will be initially empty. If @initialDataSize@
  -- is zero, @pInitialData@ is ignored.
  vkPInitialData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkPipelineCacheCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
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
