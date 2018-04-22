{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheHeaderVersionEXT(..)
  , pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
  , VkValidationCacheCreateFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT
  , pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT
  , VkValidationCacheEXT
  , vkCreateValidationCacheEXT
  , vkDestroyValidationCacheEXT
  , vkGetValidationCacheDataEXT
  , vkMergeValidationCachesEXT
  , VkValidationCacheCreateInfoEXT(..)
  , VkShaderModuleValidationCacheCreateInfoEXT(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
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
  ( VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
  )


-- ** VkValidationCacheHeaderVersionEXT

-- | VkValidationCacheHeaderVersionEXT - Encode validation cache version
--
-- = See Also
--
-- UNKNOWN:vkCreateValdiationCacheEXT, 'vkGetValidationCacheDataEXT'
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkValidationCacheHeaderVersionEXT where
  showsPrec _ VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = showString "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
  showsPrec p (VkValidationCacheHeaderVersionEXT x) = showParen (p >= 11) (showString "VkValidationCacheHeaderVersionEXT " . showsPrec 11 x)

instance Read VkValidationCacheHeaderVersionEXT where
  readPrec = parens ( choose [ ("VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT", pure VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationCacheHeaderVersionEXT")
                        v <- step readPrec
                        pure (VkValidationCacheHeaderVersionEXT v)
                        )
                    )

-- | @VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT@ specifies version one of
-- the validation cache.
pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: VkValidationCacheHeaderVersionEXT
pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = VkValidationCacheHeaderVersionEXT 1
-- ** VkValidationCacheCreateFlagsEXT

-- | VkValidationCacheCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- @VkValidationCacheCreateFlagsEXT@ is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'VkValidationCacheCreateInfoEXT'
newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkValidationCacheCreateFlagsEXT where
  
  showsPrec p (VkValidationCacheCreateFlagsEXT x) = showParen (p >= 11) (showString "VkValidationCacheCreateFlagsEXT " . showsPrec 11 x)

instance Read VkValidationCacheCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationCacheCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkValidationCacheCreateFlagsEXT v)
                        )
                    )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160001
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT = VkObjectType 1000160000
-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_SPEC_VERSION"
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_EXTENSION_NAME"
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"
-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
-- | Dummy data to tag the 'Ptr' with
data VkValidationCacheEXT_T
-- | VkValidationCacheEXT - Opaque handle to a validation cache object
--
-- = See Also
--
-- 'VkShaderModuleValidationCacheCreateInfoEXT',
-- 'vkCreateValidationCacheEXT', 'vkDestroyValidationCacheEXT',
-- 'vkGetValidationCacheDataEXT', 'vkMergeValidationCachesEXT'
type VkValidationCacheEXT = Ptr VkValidationCacheEXT_T
-- | vkCreateValidationCacheEXT - Creates a new validation cache
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the validation cache
--     object.
--
-- -   @pCreateInfo@ is a pointer to a @VkValidationCacheCreateInfoEXT@
--     structure that contains the initial parameters for the validation
--     cache object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pValidationCache@ is a pointer to a @VkValidationCacheEXT@ handle
--     in which the resulting validation cache object is returned.
--
-- = Description
--
-- __Note__
--
-- Applications /can/ track and manage the total host memory size of a
-- validation cache object using the @pAllocator@. Applications /can/ limit
-- the amount of data retrieved from a validation cache object in
-- @vkGetValidationCacheDataEXT@. Implementations /should/ not internally
-- limit the total number of entries added to a validation cache object or
-- the total host memory consumed.
--
-- Once created, a validation cache /can/ be passed to the
-- @vkCreateShaderModule@ command as part of the @VkShaderModuleCreateInfo@
-- @pNext@ chain. If a @VkShaderModuleValidationCacheCreateInfoEXT@ object
-- is part of the @VkShaderModuleCreateInfo@::@pNext@ chain, and its
-- @validationCache@ field is not
-- 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', the implementation
-- will query it for possible reuse opportunities and update it with new
-- content. The use of the validation cache object in these commands is
-- internally synchronized, and the same validation cache object /can/ be
-- used in multiple threads simultaneously.
--
-- __Note__
--
-- Implementations /should/ make every effort to limit any critical
-- sections to the actual accesses to the cache, which is expected to be
-- significantly shorter than the duration of the @vkCreateShaderModule@
-- command.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkValidationCacheCreateInfoEXT@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pValidationCache@ /must/ be a valid pointer to a
--     @VkValidationCacheEXT@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkValidationCacheCreateInfoEXT', 'VkValidationCacheEXT'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateValidationCacheEXT" vkCreateValidationCacheEXT :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult
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
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @validationCache@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @validationCache@
--     was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @validationCache@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @validationCache@
--     /must/ be a valid @VkValidationCacheEXT@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @validationCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @validationCache@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkValidationCacheEXT'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyValidationCacheEXT" vkDestroyValidationCacheEXT :: ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
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
-- @pData@, and @vkGetValidationCacheDataEXT@ will return @VK_INCOMPLETE@.
-- Any data written to @pData@ is valid and /can/ be provided as the
-- @pInitialData@ member of the @VkValidationCacheCreateInfoEXT@ structure
-- passed to @vkCreateValidationCacheEXT@.
--
-- Two calls to @vkGetValidationCacheDataEXT@ with the same parameters
-- /must/ retrieve the same data unless a command that modifies the
-- contents of the cache is called between them.
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
-- +--------+----------------+--------------------------------------------------+
-- | Offset | Size           | Meaning                                          |
-- +========+================+==================================================+
-- | 0      | 4              | length in bytes of the entire validation cache   |
-- |        |                | header written as a stream of bytes, with the    |
-- |        |                | least significant byte first                     |
-- +--------+----------------+--------------------------------------------------+
-- | 4      | 4              | a 'VkValidationCacheHeaderVersionEXT' value      |
-- |        |                | written as a stream of bytes, with the least     |
-- |        |                | significant byte first                           |
-- +--------+----------------+--------------------------------------------------+
-- | 8      | @VK_UUID_SIZE@ | a layer commit ID expressed as a UUID, which     |
-- |        |                | uniquely identifies the version of the           |
-- |        |                | validation layers used to generate these         |
-- |        |                | validation results                               |
-- +--------+----------------+--------------------------------------------------+
--
-- Layout for validation cache header version
-- @VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT@
--
-- The first four bytes encode the length of the entire validation cache
-- header, in bytes. This value includes all fields in the header including
-- the validation cache version field and the size of the length field.
--
-- The next four bytes encode the validation cache version, as described
-- for 'VkValidationCacheHeaderVersionEXT'. A consumer of the validation
-- cache /should/ use the cache version to interpret the remainder of the
-- cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @validationCache@ /must/ be a valid @VkValidationCacheEXT@ handle
--
-- -   @pDataSize@ /must/ be a valid pointer to a @size_t@ value
--
-- -   If the value referenced by @pDataSize@ is not @0@, and @pData@ is
--     not @NULL@, @pData@ /must/ be a valid pointer to an array of
--     @pDataSize@ bytes
--
-- -   @validationCache@ /must/ have been created, allocated, or retrieved
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
-- 'VkValidationCacheEXT'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetValidationCacheDataEXT" vkGetValidationCacheDataEXT :: ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
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
-- -   @dstCache@ /must/ not appear in the list of source caches
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @dstCache@ /must/ be a valid @VkValidationCacheEXT@ handle
--
-- -   @pSrcCaches@ /must/ be a valid pointer to an array of
--     @srcCacheCount@ valid @VkValidationCacheEXT@ handles
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
-- 'VkValidationCacheEXT'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMergeValidationCachesEXT" vkMergeValidationCachesEXT :: ("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult
-- | VkValidationCacheCreateInfoEXT - Structure specifying parameters of a
-- newly created validation cache
--
-- == Valid Usage
--
-- -   If @initialDataSize@ is not @0@, it /must/ be equal to the size of
--     @pInitialData@, as returned by @vkGetValidationCacheDataEXT@ when
--     @pInitialData@ was originally retrieved
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ have been
--     retrieved from a previous call to @vkGetValidationCacheDataEXT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT@
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
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkValidationCacheCreateFlagsEXT', 'vkCreateValidationCacheEXT'
data VkValidationCacheCreateInfoEXT = VkValidationCacheCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkValidationCacheCreateFlagsEXT
  , -- | @initialDataSize@ is the number of bytes in @pInitialData@. If
  -- @initialDataSize@ is zero, the validation cache will initially be empty.
  vkInitialDataSize :: CSize
  , -- | @pInitialData@ is a pointer to previously retrieved validation cache
  -- data. If the validation cache data is incompatible (as defined below)
  -- with the device, the validation cache will be initially empty. If
  -- @initialDataSize@ is zero, @pInitialData@ is ignored.
  vkPInitialData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkValidationCacheCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPInitialData (poked :: VkValidationCacheCreateInfoEXT))
-- | VkShaderModuleValidationCacheCreateInfoEXT - Specify validation cache to
-- use during shader module creation
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT@
--
-- -   @validationCache@ /must/ be a valid @VkValidationCacheEXT@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'VkValidationCacheEXT'
data VkShaderModuleValidationCacheCreateInfoEXT = VkShaderModuleValidationCacheCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @validationCache@ is the validation cache object from which the results
  -- of prior validation attempts will be written, and to which new
  -- validation results for this VkShaderModule will be written (if not
  -- already present).
  vkValidationCache :: VkValidationCacheEXT
  }
  deriving (Eq, Show)

instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkShaderModuleValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkValidationCache (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
