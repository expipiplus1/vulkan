{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( withCStructShaderModuleValidationCacheCreateInfoEXT
  , fromCStructShaderModuleValidationCacheCreateInfoEXT
  , ShaderModuleValidationCacheCreateInfoEXT(..)
  , ValidationCacheCreateFlagsEXT
  , withCStructValidationCacheCreateInfoEXT
  , fromCStructValidationCacheCreateInfoEXT
  , ValidationCacheCreateInfoEXT(..)
  , ValidationCacheEXT
  , ValidationCacheHeaderVersionEXT
  , pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
  , createValidationCacheEXT
  , destroyValidationCacheEXT
  , getNumValidationCacheDataEXT
  , getValidationCacheDataEXT
  , getAllValidationCacheDataEXT
  , mergeValidationCachesEXT
  , withValidationCacheEXT
  , pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT
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
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateFlagsEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , VkValidationCacheHeaderVersionEXT(..)
  , VkValidationCacheEXT
  , vkCreateValidationCacheEXT
  , vkDestroyValidationCacheEXT
  , vkGetValidationCacheDataEXT
  , vkMergeValidationCachesEXT
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT
  )



-- | VkShaderModuleValidationCacheCreateInfoEXT - Specify validation cache to
-- use during shader module creation
--
-- = Description
--
-- Unresolved directive in VkShaderModuleValidationCacheCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkShaderModuleValidationCacheCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ShaderModuleValidationCacheCreateInfoEXT = ShaderModuleValidationCacheCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ShaderModuleValidationCacheCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ShaderModuleValidationCacheCreateInfoEXT" "validationCache"
  validationCache :: ValidationCacheEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkShaderModuleValidationCacheCreateInfoEXT' and
-- marshal a 'ShaderModuleValidationCacheCreateInfoEXT' into it. The 'VkShaderModuleValidationCacheCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructShaderModuleValidationCacheCreateInfoEXT :: ShaderModuleValidationCacheCreateInfoEXT -> (VkShaderModuleValidationCacheCreateInfoEXT -> IO a) -> IO a
withCStructShaderModuleValidationCacheCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ShaderModuleValidationCacheCreateInfoEXT)) (\pPNext -> cont (VkShaderModuleValidationCacheCreateInfoEXT VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT pPNext (validationCache (marshalled :: ShaderModuleValidationCacheCreateInfoEXT))))

-- | A function to read a 'VkShaderModuleValidationCacheCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'ShaderModuleValidationCacheCreateInfoEXT'.
fromCStructShaderModuleValidationCacheCreateInfoEXT :: VkShaderModuleValidationCacheCreateInfoEXT -> IO ShaderModuleValidationCacheCreateInfoEXT
fromCStructShaderModuleValidationCacheCreateInfoEXT c = ShaderModuleValidationCacheCreateInfoEXT <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkShaderModuleValidationCacheCreateInfoEXT)))
                                                                                                 <*> pure (vkValidationCache (c :: VkShaderModuleValidationCacheCreateInfoEXT))

instance Zero ShaderModuleValidationCacheCreateInfoEXT where
  zero = ShaderModuleValidationCacheCreateInfoEXT Nothing
                                                  zero


-- | VkValidationCacheCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- No cross-references are available
type ValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT


-- | VkValidationCacheCreateInfoEXT - Structure specifying parameters of a
-- newly created validation cache
--
-- == Valid Usage
--
-- -   If @initialDataSize@ is not @0@, it /must/ be equal to the size of
--     @pInitialData@, as returned by
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
--     when @pInitialData@ was originally retrieved
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ have been
--     retrieved from a previous call to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
--
-- Unresolved directive in VkValidationCacheCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkValidationCacheCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ValidationCacheCreateInfoEXT = ValidationCacheCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "flags"
  flags :: ValidationCacheCreateFlagsEXT
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "pInitialData"
  initialData :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkValidationCacheCreateInfoEXT' and
-- marshal a 'ValidationCacheCreateInfoEXT' into it. The 'VkValidationCacheCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructValidationCacheCreateInfoEXT :: ValidationCacheCreateInfoEXT -> (VkValidationCacheCreateInfoEXT -> IO a) -> IO a
withCStructValidationCacheCreateInfoEXT marshalled cont = unsafeUseAsCString (initialData (marshalled :: ValidationCacheCreateInfoEXT)) (\pPInitialData -> maybeWith withSomeVkStruct (next (marshalled :: ValidationCacheCreateInfoEXT)) (\pPNext -> cont (VkValidationCacheCreateInfoEXT VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT pPNext (flags (marshalled :: ValidationCacheCreateInfoEXT)) (fromIntegral (Data.ByteString.length (initialData (marshalled :: ValidationCacheCreateInfoEXT)))) (castPtr pPInitialData))))

-- | A function to read a 'VkValidationCacheCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'ValidationCacheCreateInfoEXT'.
fromCStructValidationCacheCreateInfoEXT :: VkValidationCacheCreateInfoEXT -> IO ValidationCacheCreateInfoEXT
fromCStructValidationCacheCreateInfoEXT c = ValidationCacheCreateInfoEXT <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkValidationCacheCreateInfoEXT)))
                                                                         <*> pure (vkFlags (c :: VkValidationCacheCreateInfoEXT))
                                                                         -- Bytestring length valued member elided
                                                                         <*> packCStringLen (castPtr (vkPInitialData (c :: VkValidationCacheCreateInfoEXT)), fromIntegral (vkInitialDataSize (c :: VkValidationCacheCreateInfoEXT)))

instance Zero ValidationCacheCreateInfoEXT where
  zero = ValidationCacheCreateInfoEXT Nothing
                                      zero
                                      Data.ByteString.empty


-- | VkValidationCacheEXT - Opaque handle to a validation cache object
--
-- = See Also
--
-- No cross-references are available
type ValidationCacheEXT = VkValidationCacheEXT

-- | VkValidationCacheHeaderVersionEXT - Encode validation cache version
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
type ValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT'
-- specifies version one of the validation cache.
pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: (a ~ ValidationCacheHeaderVersionEXT) => a
pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT


-- | vkCreateValidationCacheEXT - Creates a new validation cache
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the validation cache
--     object.
--
-- -   @pCreateInfo@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateInfoEXT'
--     structure that contains the initial parameters for the validation
--     cache object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pValidationCache@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheEXT'
--     handle in which the resulting validation cache object is returned.
--
-- = Description
--
-- __Note__
--
-- Applications /can/ track and manage the total host memory size of a
-- validation cache object using the @pAllocator@. Applications /can/ limit
-- the amount of data retrieved from a validation cache object in
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'.
-- Implementations /should/ not internally limit the total number of
-- entries added to a validation cache object or the total host memory
-- consumed.
--
-- Once created, a validation cache /can/ be passed to the
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule' command as part
-- of the 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo'
-- @pNext@ chain. If a
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkShaderModuleValidationCacheCreateInfoEXT'
-- object is part of the
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo'::@pNext@
-- chain, and its @validationCache@ field is not
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the implementation
-- will query it for possible reuse opportunities and update it with new
-- content. The use of the validation cache object in these commands is
-- internally synchronized, and the same validation cache object /can/ be
-- used in multiple threads simultaneously.
--
-- __Note__
--
-- Implementations /should/ make every effort to limit any critical
-- sections to the actual accesses to the cache, which is expected to be
-- significantly shorter than the duration of the
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule' command.
--
-- Unresolved directive in vkCreateValidationCacheEXT.txt -
-- include::{generated}\/validity\/protos\/vkCreateValidationCacheEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
createValidationCacheEXT :: Device ->  ValidationCacheCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (ValidationCacheEXT)
createValidationCacheEXT = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pValidationCache' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructValidationCacheCreateInfoEXT marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateValidationCacheEXT commandTable device' pCreateInfo' pAllocator pValidationCache' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pValidationCache')))))


-- | vkDestroyValidationCacheEXT - Destroy a validation cache object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the validation cache
--     object.
--
-- -   @validationCache@ is the handle of the validation cache to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @validationCache@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @validationCache@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- Unresolved directive in vkDestroyValidationCacheEXT.txt -
-- include::{generated}\/validity\/protos\/vkDestroyValidationCacheEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
destroyValidationCacheEXT :: Device ->  ValidationCacheEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyValidationCacheEXT = \(Device device' commandTable) -> \validationCache' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyValidationCacheEXT commandTable device' validationCache' pAllocator *> (pure ()))


-- | vkGetValidationCacheDataEXT - Get the data store from a validation cache
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the validation cache.
--
-- -   @validationCache@ is the validation cache to retrieve data from.
--
-- -   @pDataSize@ is a pointer to a value related to the amount of data in
--     the validation cache, as described below.
--
-- -   @pData@ is either @NULL@ or a pointer to a buffer.
--
-- = Description
--
-- If @pData@ is @NULL@, then the maximum size of the data that /can/ be
-- retrieved from the validation cache, in bytes, is returned in
-- @pDataSize@. Otherwise, @pDataSize@ /must/ point to a variable set by
-- the user to the size of the buffer, in bytes, pointed to by @pData@, and
-- on return the variable is overwritten with the amount of data actually
-- written to @pData@.
--
-- If @pDataSize@ is less than the maximum size that /can/ be retrieved by
-- the validation cache, at most @pDataSize@ bytes will be written to
-- @pData@, and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
-- will return 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'. Any data
-- written to @pData@ is valid and /can/ be provided as the @pInitialData@
-- member of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateInfoEXT'
-- structure passed to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT'.
--
-- Two calls to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
-- with the same parameters /must/ retrieve the same data unless a command
-- that modifies the contents of the cache is called between them.
--
-- Applications /can/ store the data retrieved from the validation cache,
-- and use these data, possibly in a future run of the application, to
-- populate new validation cache objects. The results of validation,
-- however, /may/ depend on the vendor ID, device ID, driver version, and
-- other details of the device. To enable applications to detect when
-- previously retrieved data is incompatible with the device, the initial
-- bytes written to @pData@ /must/ be a header consisting of the following
-- members:
--
-- > +----+--------------+--------------------------------------------------+
-- > | Of | Size         | Meaning                                          |
-- > | fs |              |                                                  |
-- > | et |              |                                                  |
-- > +====+==============+==================================================+
-- > | 0  | 4            | length in bytes of the entire validation cache   |
-- > |    |              | header written as a stream of bytes, with the    |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 4  | 4            | a                                                |
-- > |    |              | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_ |
-- > |    |              | cache.VkValidationCacheHeaderVersionEXT'         |
-- > |    |              | value written as a stream of bytes, with the     |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 8  | 'Graphics.Vu | a layer commit ID expressed as a UUID, which     |
-- > |    | lkan.C.Core1 | uniquely identifies the version of the           |
-- > |    | 0.DeviceInit | validation layers used to generate these         |
-- > |    | ialization.V | validation results                               |
-- > |    | K_UUID_SIZE' |                                                  |
-- > +----+--------------+--------------------------------------------------+
-- >
-- > Layout for validation cache header version
-- > 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT'
--
-- The first four bytes encode the length of the entire validation cache
-- header, in bytes. This value includes all fields in the header including
-- the validation cache version field and the size of the length field.
--
-- The next four bytes encode the validation cache version, as described
-- for
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheHeaderVersionEXT'.
-- A consumer of the validation cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- Unresolved directive in vkGetValidationCacheDataEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetValidationCacheDataEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  IO (VkResult, CSize)
getNumValidationCacheDataEXT = \(Device device' commandTable) -> \validationCache' -> alloca (\pDataSize' -> vkGetValidationCacheDataEXT commandTable device' validationCache' pDataSize' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pDataSize')))

-- | vkGetValidationCacheDataEXT - Get the data store from a validation cache
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the validation cache.
--
-- -   @validationCache@ is the validation cache to retrieve data from.
--
-- -   @pDataSize@ is a pointer to a value related to the amount of data in
--     the validation cache, as described below.
--
-- -   @pData@ is either @NULL@ or a pointer to a buffer.
--
-- = Description
--
-- If @pData@ is @NULL@, then the maximum size of the data that /can/ be
-- retrieved from the validation cache, in bytes, is returned in
-- @pDataSize@. Otherwise, @pDataSize@ /must/ point to a variable set by
-- the user to the size of the buffer, in bytes, pointed to by @pData@, and
-- on return the variable is overwritten with the amount of data actually
-- written to @pData@.
--
-- If @pDataSize@ is less than the maximum size that /can/ be retrieved by
-- the validation cache, at most @pDataSize@ bytes will be written to
-- @pData@, and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
-- will return 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'. Any data
-- written to @pData@ is valid and /can/ be provided as the @pInitialData@
-- member of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateInfoEXT'
-- structure passed to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT'.
--
-- Two calls to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
-- with the same parameters /must/ retrieve the same data unless a command
-- that modifies the contents of the cache is called between them.
--
-- Applications /can/ store the data retrieved from the validation cache,
-- and use these data, possibly in a future run of the application, to
-- populate new validation cache objects. The results of validation,
-- however, /may/ depend on the vendor ID, device ID, driver version, and
-- other details of the device. To enable applications to detect when
-- previously retrieved data is incompatible with the device, the initial
-- bytes written to @pData@ /must/ be a header consisting of the following
-- members:
--
-- > +----+--------------+--------------------------------------------------+
-- > | Of | Size         | Meaning                                          |
-- > | fs |              |                                                  |
-- > | et |              |                                                  |
-- > +====+==============+==================================================+
-- > | 0  | 4            | length in bytes of the entire validation cache   |
-- > |    |              | header written as a stream of bytes, with the    |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 4  | 4            | a                                                |
-- > |    |              | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_ |
-- > |    |              | cache.VkValidationCacheHeaderVersionEXT'         |
-- > |    |              | value written as a stream of bytes, with the     |
-- > |    |              | least significant byte first                     |
-- > +----+--------------+--------------------------------------------------+
-- > | 8  | 'Graphics.Vu | a layer commit ID expressed as a UUID, which     |
-- > |    | lkan.C.Core1 | uniquely identifies the version of the           |
-- > |    | 0.DeviceInit | validation layers used to generate these         |
-- > |    | ialization.V | validation results                               |
-- > |    | K_UUID_SIZE' |                                                  |
-- > +----+--------------+--------------------------------------------------+
-- >
-- > Layout for validation cache header version
-- > 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT'
--
-- The first four bytes encode the length of the entire validation cache
-- header, in bytes. This value includes all fields in the header including
-- the validation cache version field and the size of the length field.
--
-- The next four bytes encode the validation cache version, as described
-- for
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheHeaderVersionEXT'.
-- A consumer of the validation cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- Unresolved directive in vkGetValidationCacheDataEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetValidationCacheDataEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  CSize ->  IO (VkResult, ByteString)
getValidationCacheDataEXT = \(Device device' commandTable) -> \validationCache' -> \dataSize' -> allocaArray (fromIntegral dataSize') (\pData' -> with dataSize' (\pDataSize' -> vkGetValidationCacheDataEXT commandTable device' validationCache' pDataSize' (castPtr pData') >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(curry packCStringLen pData' =<< (fromIntegral <$> (peek pDataSize')))))))
-- | Returns all the values available from 'getValidationCacheDataEXT'.
getAllValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  IO (ByteString)
getAllValidationCacheDataEXT device' validationCache' =
  snd <$> getNumValidationCacheDataEXT device' validationCache'
    >>= \num -> snd <$> getValidationCacheDataEXT device' validationCache' num



-- | vkMergeValidationCachesEXT - Combine the data stores of validation
-- caches
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the validation cache
--     objects.
--
-- -   @dstCache@ is the handle of the validation cache to merge results
--     into.
--
-- -   @srcCacheCount@ is the length of the @pSrcCaches@ array.
--
-- -   @pSrcCaches@ is an array of validation cache handles, which will be
--     merged into @dstCache@. The previous contents of @dstCache@ are
--     included after the merge.
--
-- = Description
--
-- __Note__
--
-- The details of the merge operation are implementation dependent, but
-- implementations /should/ merge the contents of the specified validation
-- caches and prune duplicate entries.
--
-- == Valid Usage
--
-- Unresolved directive in vkMergeValidationCachesEXT.txt -
-- include::{generated}\/validity\/protos\/vkMergeValidationCachesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
mergeValidationCachesEXT :: Device ->  ValidationCacheEXT ->  Vector ValidationCacheEXT ->  IO ()
mergeValidationCachesEXT = \(Device device' commandTable) -> \dstCache' -> \srcCaches' -> withVec (&) srcCaches' (\pSrcCaches' -> vkMergeValidationCachesEXT commandTable device' dstCache' (fromIntegral $ Data.Vector.length srcCaches') pSrcCaches' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))

-- | A safe wrapper for 'createValidationCacheEXT' and 'destroyValidationCacheEXT' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withValidationCacheEXT
  :: Device -> ValidationCacheCreateInfoEXT -> Maybe (AllocationCallbacks) -> (ValidationCacheEXT -> IO a) -> IO a
withValidationCacheEXT device validationCacheCreateInfoEXT allocationCallbacks = bracket
  (createValidationCacheEXT device validationCacheCreateInfoEXT allocationCallbacks)
  (\o -> destroyValidationCacheEXT device o allocationCallbacks)
