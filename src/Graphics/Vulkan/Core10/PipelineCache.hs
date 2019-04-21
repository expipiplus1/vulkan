{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  , PipelineCacheCreateFlags
  , withCStructPipelineCacheCreateInfo
  , fromCStructPipelineCacheCreateInfo
  , PipelineCacheCreateInfo(..)
  , createPipelineCache
  , destroyPipelineCache
  , getNumPipelineCacheData
  , getPipelineCacheData
  , getAllPipelineCacheData
  , mergePipelineCaches
  , withPipelineCache
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import qualified Data.ByteString
  ( empty
  , length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags(..)
  , VkPipelineCacheCreateInfo(..)
  , VkPipelineCache
  , vkCreatePipelineCache
  , vkDestroyPipelineCache
  , vkGetPipelineCacheData
  , vkMergePipelineCaches
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkPipelineCache - Opaque handle to a pipeline cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkDestroyPipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkMergePipelineCaches'
type PipelineCache = VkPipelineCache

-- | VkPipelineCacheCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo'
type PipelineCacheCreateFlags = VkPipelineCacheCreateFlags


-- | VkPipelineCacheCreateInfo - Structure specifying parameters of a newly
-- created pipeline cache
--
-- == Valid Usage
--
-- -   If @initialDataSize@ is not @0@, it /must/ be equal to the size of
--     @pInitialData@, as returned by
--     'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData' when
--     @pInitialData@ was originally retrieved
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ have been
--     retrieved from a previous call to
--     'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData'
--
-- Unresolved directive in VkPipelineCacheCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineCacheCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache'
data PipelineCacheCreateInfo = PipelineCacheCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineCacheCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCacheCreateInfo" "flags"
  flags :: PipelineCacheCreateFlags
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "PipelineCacheCreateInfo" "pInitialData"
  initialData :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineCacheCreateInfo' and
-- marshal a 'PipelineCacheCreateInfo' into it. The 'VkPipelineCacheCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineCacheCreateInfo :: PipelineCacheCreateInfo -> (VkPipelineCacheCreateInfo -> IO a) -> IO a
withCStructPipelineCacheCreateInfo marshalled cont = unsafeUseAsCString (initialData (marshalled :: PipelineCacheCreateInfo)) (\pPInitialData -> maybeWith withSomeVkStruct (next (marshalled :: PipelineCacheCreateInfo)) (\pPNext -> cont (VkPipelineCacheCreateInfo VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO pPNext (flags (marshalled :: PipelineCacheCreateInfo)) (fromIntegral (Data.ByteString.length (initialData (marshalled :: PipelineCacheCreateInfo)))) (castPtr pPInitialData))))

-- | A function to read a 'VkPipelineCacheCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineCacheCreateInfo'.
fromCStructPipelineCacheCreateInfo :: VkPipelineCacheCreateInfo -> IO PipelineCacheCreateInfo
fromCStructPipelineCacheCreateInfo c = PipelineCacheCreateInfo <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCacheCreateInfo)))
                                                               <*> pure (vkFlags (c :: VkPipelineCacheCreateInfo))
                                                               -- Bytestring length valued member elided
                                                               <*> packCStringLen (castPtr (vkPInitialData (c :: VkPipelineCacheCreateInfo)), fromIntegral (vkInitialDataSize (c :: VkPipelineCacheCreateInfo)))

instance Zero PipelineCacheCreateInfo where
  zero = PipelineCacheCreateInfo Nothing
                                 zero
                                 Data.ByteString.empty



-- | vkCreatePipelineCache - Creates a new pipeline cache
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the pipeline cache
--     object.
--
-- -   @pCreateInfo@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo'
--     structure that contains the initial parameters for the pipeline
--     cache object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelineCache@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache' handle in
--     which the resulting pipeline cache object is returned.
--
-- = Description
--
-- __Note__
--
-- Applications /can/ track and manage the total host memory size of a
-- pipeline cache object using the @pAllocator@. Applications /can/ limit
-- the amount of data retrieved from a pipeline cache object in
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData'.
-- Implementations /should/ not internally limit the total number of
-- entries added to a pipeline cache object or the total host memory
-- consumed.
--
-- Once created, a pipeline cache /can/ be passed to the
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines' and
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines' commands.
-- If the pipeline cache passed into these commands is not
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the implementation
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
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines' and
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines' commands.
--
-- Unresolved directive in vkCreatePipelineCache.txt -
-- include::{generated}\/validity\/protos\/vkCreatePipelineCache.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo'
createPipelineCache :: Device ->  PipelineCacheCreateInfo ->  Maybe AllocationCallbacks ->  IO (PipelineCache)
createPipelineCache = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pPipelineCache' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructPipelineCacheCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreatePipelineCache commandTable device' pCreateInfo' pAllocator pPipelineCache' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pPipelineCache')))))


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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @pipelineCache@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @pipelineCache@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- Unresolved directive in vkDestroyPipelineCache.txt -
-- include::{generated}\/validity\/protos\/vkDestroyPipelineCache.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache'
destroyPipelineCache :: Device ->  PipelineCache ->  Maybe AllocationCallbacks ->  IO ()
destroyPipelineCache = \(Device device' commandTable) -> \pipelineCache' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyPipelineCache commandTable device' pipelineCache' pAllocator *> (pure ()))


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
-- @pData@, and
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData' will
-- return 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'. Any data written
-- to @pData@ is valid and /can/ be provided as the @pInitialData@ member
-- of the
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo'
-- structure passed to
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache'.
--
-- Two calls to
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData' with the
-- same parameters /must/ retrieve the same data unless a command that
-- modifies the contents of the cache is called between them.
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
-- > +----+--------------+--------------------------------------------------+
-- > | Of | Size         | Meaning                                          |
-- > | fs |              |                                                  |
-- > | et |              |                                                  |
-- > +====+==============+==================================================+
-- > | 0  | 4            | length in bytes of the entire pipeline cache     |
-- > |    |              | header written as a stream of bytes, with the    |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 4  | 4            | a                                                |
-- > |    |              | 'Graphics.Vulkan.C.Core10.Constants.VkPipelineCa |
-- > |    |              | cheHeaderVersion'                                |
-- > |    |              | value written as a stream of bytes, with the     |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 8  | 4            | a vendor ID equal to                             |
-- > |    |              | 'Graphics.Vulkan.C.Core10.DeviceInitialization.V |
-- > |    |              | kPhysicalDeviceProperties'::@vendorID@           |
-- > |    |              | written as a stream of bytes, with the least     |
-- > |    |              | significant byte first                           |
-- > +----+--------------+--------------------------------------------------+
-- > | 12 | 4            | a device ID equal to                             |
-- > |    |              | 'Graphics.Vulkan.C.Core10.DeviceInitialization.V |
-- > |    |              | kPhysicalDeviceProperties'::@deviceID@           |
-- > |    |              | written as a stream of bytes, with the least     |
-- > |    |              | significant byte first                           |
-- > +----+--------------+--------------------------------------------------+
-- > | 16 | 'Graphics.Vu | a pipeline cache ID equal to                     |
-- > |    | lkan.C.Core1 | 'Graphics.Vulkan.C.Core10.DeviceInitialization.V |
-- > |    | 0.DeviceInit | kPhysicalDeviceProperties'::@pipelineCacheUUID@  |
-- > |    | ialization.V |                                                  |
-- > |    | K_UUID_SIZE' |                                                  |
-- > +----+--------------+--------------------------------------------------+
-- >
-- > Layout for pipeline cache header version
-- > 'Graphics.Vulkan.C.Core10.Constants.VK_PIPELINE_CACHE_HEADER_VERSION_ONE'
--
-- The first four bytes encode the length of the entire pipeline cache
-- header, in bytes. This value includes all fields in the header including
-- the pipeline cache version field and the size of the length field.
--
-- The next four bytes encode the pipeline cache version, as described for
-- 'Graphics.Vulkan.C.Core10.Constants.VkPipelineCacheHeaderVersion'. A
-- consumer of the pipeline cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- Unresolved directive in vkGetPipelineCacheData.txt -
-- include::{generated}\/validity\/protos\/vkGetPipelineCacheData.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache'
getNumPipelineCacheData :: Device ->  PipelineCache ->  IO (VkResult, CSize)
getNumPipelineCacheData = \(Device device' commandTable) -> \pipelineCache' -> alloca (\pDataSize' -> vkGetPipelineCacheData commandTable device' pipelineCache' pDataSize' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pDataSize')))

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
-- @pData@, and
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData' will
-- return 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'. Any data written
-- to @pData@ is valid and /can/ be provided as the @pInitialData@ member
-- of the
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo'
-- structure passed to
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache'.
--
-- Two calls to
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData' with the
-- same parameters /must/ retrieve the same data unless a command that
-- modifies the contents of the cache is called between them.
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
-- > +----+--------------+--------------------------------------------------+
-- > | Of | Size         | Meaning                                          |
-- > | fs |              |                                                  |
-- > | et |              |                                                  |
-- > +====+==============+==================================================+
-- > | 0  | 4            | length in bytes of the entire pipeline cache     |
-- > |    |              | header written as a stream of bytes, with the    |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 4  | 4            | a                                                |
-- > |    |              | 'Graphics.Vulkan.C.Core10.Constants.VkPipelineCa |
-- > |    |              | cheHeaderVersion'                                |
-- > |    |              | value written as a stream of bytes, with the     |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 8  | 4            | a vendor ID equal to                             |
-- > |    |              | 'Graphics.Vulkan.C.Core10.DeviceInitialization.V |
-- > |    |              | kPhysicalDeviceProperties'::@vendorID@           |
-- > |    |              | written as a stream of bytes, with the least     |
-- > |    |              | significant byte first                           |
-- > +----+--------------+--------------------------------------------------+
-- > | 12 | 4            | a device ID equal to                             |
-- > |    |              | 'Graphics.Vulkan.C.Core10.DeviceInitialization.V |
-- > |    |              | kPhysicalDeviceProperties'::@deviceID@           |
-- > |    |              | written as a stream of bytes, with the least     |
-- > |    |              | significant byte first                           |
-- > +----+--------------+--------------------------------------------------+
-- > | 16 | 'Graphics.Vu | a pipeline cache ID equal to                     |
-- > |    | lkan.C.Core1 | 'Graphics.Vulkan.C.Core10.DeviceInitialization.V |
-- > |    | 0.DeviceInit | kPhysicalDeviceProperties'::@pipelineCacheUUID@  |
-- > |    | ialization.V |                                                  |
-- > |    | K_UUID_SIZE' |                                                  |
-- > +----+--------------+--------------------------------------------------+
-- >
-- > Layout for pipeline cache header version
-- > 'Graphics.Vulkan.C.Core10.Constants.VK_PIPELINE_CACHE_HEADER_VERSION_ONE'
--
-- The first four bytes encode the length of the entire pipeline cache
-- header, in bytes. This value includes all fields in the header including
-- the pipeline cache version field and the size of the length field.
--
-- The next four bytes encode the pipeline cache version, as described for
-- 'Graphics.Vulkan.C.Core10.Constants.VkPipelineCacheHeaderVersion'. A
-- consumer of the pipeline cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- Unresolved directive in vkGetPipelineCacheData.txt -
-- include::{generated}\/validity\/protos\/vkGetPipelineCacheData.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache'
getPipelineCacheData :: Device ->  PipelineCache ->  CSize ->  IO (VkResult, ByteString)
getPipelineCacheData = \(Device device' commandTable) -> \pipelineCache' -> \dataSize' -> allocaArray (fromIntegral dataSize') (\pData' -> with dataSize' (\pDataSize' -> vkGetPipelineCacheData commandTable device' pipelineCache' pDataSize' (castPtr pData') >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(curry packCStringLen pData' =<< (fromIntegral <$> (peek pDataSize')))))))
-- | Returns all the values available from 'getPipelineCacheData'.
getAllPipelineCacheData :: Device ->  PipelineCache ->  IO (ByteString)
getAllPipelineCacheData device' pipelineCache' =
  snd <$> getNumPipelineCacheData device' pipelineCache'
    >>= \num -> snd <$> getPipelineCacheData device' pipelineCache' num



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
-- Unresolved directive in vkMergePipelineCaches.txt -
-- include::{generated}\/validity\/protos\/vkMergePipelineCaches.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache'
mergePipelineCaches :: Device ->  PipelineCache ->  Vector PipelineCache ->  IO ()
mergePipelineCaches = \(Device device' commandTable) -> \dstCache' -> \srcCaches' -> withVec (&) srcCaches' (\pSrcCaches' -> vkMergePipelineCaches commandTable device' dstCache' (fromIntegral $ Data.Vector.length srcCaches') pSrcCaches' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))

-- | A safe wrapper for 'createPipelineCache' and 'destroyPipelineCache' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withPipelineCache
  :: Device -> PipelineCacheCreateInfo -> Maybe (AllocationCallbacks) -> (PipelineCache -> IO a) -> IO a
withPipelineCache device pipelineCacheCreateInfo allocationCallbacks = bracket
  (createPipelineCache device pipelineCacheCreateInfo allocationCallbacks)
  (\o -> destroyPipelineCache device o allocationCallbacks)
