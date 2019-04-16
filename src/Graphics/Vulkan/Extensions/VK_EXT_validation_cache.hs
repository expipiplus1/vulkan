{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

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
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import qualified Data.ByteString
  ( length
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
import qualified Graphics.Vulkan.C.Dynamic
  ( createValidationCacheEXT
  , destroyValidationCacheEXT
  , getValidationCacheDataEXT
  , mergeValidationCachesEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateFlagsEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , VkValidationCacheHeaderVersionEXT(..)
  , VkValidationCacheEXT
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
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


-- No documentation found for TopLevel "ShaderModuleValidationCacheCreateInfoEXT"
data ShaderModuleValidationCacheCreateInfoEXT = ShaderModuleValidationCacheCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ShaderModuleValidationCacheCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ShaderModuleValidationCacheCreateInfoEXT" "validationCache"
  vkValidationCache :: ValidationCacheEXT
  }
  deriving (Show, Eq)
withCStructShaderModuleValidationCacheCreateInfoEXT :: ShaderModuleValidationCacheCreateInfoEXT -> (VkShaderModuleValidationCacheCreateInfoEXT -> IO a) -> IO a
withCStructShaderModuleValidationCacheCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: ShaderModuleValidationCacheCreateInfoEXT)) (\pPNext -> cont (VkShaderModuleValidationCacheCreateInfoEXT VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT pPNext (vkValidationCache (from :: ShaderModuleValidationCacheCreateInfoEXT))))
fromCStructShaderModuleValidationCacheCreateInfoEXT :: VkShaderModuleValidationCacheCreateInfoEXT -> IO ShaderModuleValidationCacheCreateInfoEXT
fromCStructShaderModuleValidationCacheCreateInfoEXT c = ShaderModuleValidationCacheCreateInfoEXT <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkShaderModuleValidationCacheCreateInfoEXT)))
                                                                                                 <*> pure (vkValidationCache (c :: VkShaderModuleValidationCacheCreateInfoEXT))
-- No documentation found for TopLevel "ValidationCacheCreateFlagsEXT"
type ValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT
-- No documentation found for TopLevel "ValidationCacheCreateInfoEXT"
data ValidationCacheCreateInfoEXT = ValidationCacheCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "flags"
  vkFlags :: ValidationCacheCreateFlagsEXT
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "pInitialData"
  vkPInitialData :: ByteString
  }
  deriving (Show, Eq)
withCStructValidationCacheCreateInfoEXT :: ValidationCacheCreateInfoEXT -> (VkValidationCacheCreateInfoEXT -> IO a) -> IO a
withCStructValidationCacheCreateInfoEXT from cont = unsafeUseAsCString (vkPInitialData (from :: ValidationCacheCreateInfoEXT)) (\pInitialData -> maybeWith withSomeVkStruct (vkPNext (from :: ValidationCacheCreateInfoEXT)) (\pPNext -> cont (VkValidationCacheCreateInfoEXT VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT pPNext (vkFlags (from :: ValidationCacheCreateInfoEXT)) (fromIntegral (Data.ByteString.length (vkPInitialData (from :: ValidationCacheCreateInfoEXT)))) (castPtr pInitialData))))
fromCStructValidationCacheCreateInfoEXT :: VkValidationCacheCreateInfoEXT -> IO ValidationCacheCreateInfoEXT
fromCStructValidationCacheCreateInfoEXT c = ValidationCacheCreateInfoEXT <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkValidationCacheCreateInfoEXT)))
                                                                         <*> pure (vkFlags (c :: VkValidationCacheCreateInfoEXT))
                                                                         -- Bytestring length valued member elided
                                                                         <*> packCStringLen (castPtr (vkPInitialData (c :: VkValidationCacheCreateInfoEXT)), fromIntegral (vkInitialDataSize (c :: VkValidationCacheCreateInfoEXT)))
-- No documentation found for TopLevel "ValidationCacheEXT"
type ValidationCacheEXT = VkValidationCacheEXT
-- No documentation found for TopLevel "ValidationCacheHeaderVersionEXT"
type ValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT

-- | Wrapper for 'vkCreateValidationCacheEXT'
createValidationCacheEXT :: Device ->  ValidationCacheCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO ( ValidationCacheEXT )
createValidationCacheEXT = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pValidationCache -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructValidationCacheCreateInfoEXT a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createValidationCacheEXT commandTable device pCreateInfo pAllocator pValidationCache >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pValidationCache)))))

-- | Wrapper for 'vkDestroyValidationCacheEXT'
destroyValidationCacheEXT :: Device ->  ValidationCacheEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyValidationCacheEXT = \(Device device commandTable) -> \validationCache -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyValidationCacheEXT commandTable device validationCache pAllocator *> (pure ()))

-- | Wrapper for 'vkGetValidationCacheDataEXT'
getNumValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  IO (VkResult, CSize)
getNumValidationCacheDataEXT = \(Device device commandTable) -> \validationCache -> alloca (\pDataSize -> Graphics.Vulkan.C.Dynamic.getValidationCacheDataEXT commandTable device validationCache pDataSize nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pDataSize)))

-- | Wrapper for 'vkGetValidationCacheDataEXT'
getValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  CSize ->  IO (VkResult, ByteString)
getValidationCacheDataEXT = \(Device device commandTable) -> \validationCache -> \dataSize -> allocaArray (fromIntegral dataSize) (\pData -> with dataSize (\pDataSize -> Graphics.Vulkan.C.Dynamic.getValidationCacheDataEXT commandTable device validationCache pDataSize (castPtr pData) >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(curry packCStringLen pData =<< (fromIntegral <$> (peek pDataSize)))))))
-- | Call 'getNumValidationCacheDataEXT' to get the number of return values, then use that
-- number to call 'getValidationCacheDataEXT' to get all the values.
getAllValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  IO (ByteString)
getAllValidationCacheDataEXT device validationCache =
  snd <$> getNumValidationCacheDataEXT device validationCache
    >>= \num -> snd <$> getValidationCacheDataEXT device validationCache num


-- | Wrapper for 'vkMergeValidationCachesEXT'
mergeValidationCachesEXT :: Device ->  ValidationCacheEXT ->  Vector ValidationCacheEXT ->  IO ()
mergeValidationCachesEXT = \(Device device commandTable) -> \dstCache -> \srcCaches -> withVec (&) srcCaches (\pSrcCaches -> Graphics.Vulkan.C.Dynamic.mergeValidationCachesEXT commandTable device dstCache (fromIntegral $ Data.Vector.length srcCaches) pSrcCaches >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
withValidationCacheEXT :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withValidationCacheEXT createInfo allocationCallbacks =
  bracket
    (vkCreateValidationCacheEXT createInfo allocationCallbacks)
    (`vkDestroyValidationCacheEXT` allocationCallbacks)
