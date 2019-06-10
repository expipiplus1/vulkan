{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ShaderModuleValidationCacheCreateInfoEXT(..)
  , 
#endif
  ValidationCacheCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , ValidationCacheCreateInfoEXT(..)
#endif
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
  , pattern EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern OBJECT_TYPE_VALIDATION_CACHE_EXT
  ) where

import Control.Exception
  ( bracket
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import Data.Function
  ( (&)
  )
import Data.String
  ( IsString
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
  ( maybeWith
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
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateFlagsEXT(..)
  , VkValidationCacheHeaderVersionEXT(..)
  , VkValidationCacheEXT
  , vkCreateValidationCacheEXT
  , vkDestroyValidationCacheEXT
  , vkGetValidationCacheDataEXT
  , vkMergeValidationCachesEXT
  , pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION
  , pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_VALIDATION_CACHE_EXT
  , pattern STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkShaderModuleValidationCacheCreateInfoEXT"
data ShaderModuleValidationCacheCreateInfoEXT = ShaderModuleValidationCacheCreateInfoEXT
  { -- No documentation found for Nested "ShaderModuleValidationCacheCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ShaderModuleValidationCacheCreateInfoEXT" "validationCache"
  validationCache :: ValidationCacheEXT
  }
  deriving (Show, Eq)

instance Zero ShaderModuleValidationCacheCreateInfoEXT where
  zero = ShaderModuleValidationCacheCreateInfoEXT Nothing
                                                  zero

#endif

-- No documentation found for TopLevel "ValidationCacheCreateFlagsEXT"
type ValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT


-- No complete pragma for ValidationCacheCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkValidationCacheCreateInfoEXT"
data ValidationCacheCreateInfoEXT = ValidationCacheCreateInfoEXT
  { -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "flags"
  flags :: ValidationCacheCreateFlagsEXT
  , -- No documentation found for Nested "ValidationCacheCreateInfoEXT" "pInitialData"
  initialData :: ByteString
  }
  deriving (Show, Eq)

instance Zero ValidationCacheCreateInfoEXT where
  zero = ValidationCacheCreateInfoEXT Nothing
                                      zero
                                      mempty

#endif

-- No documentation found for TopLevel "ValidationCacheEXT"
type ValidationCacheEXT = VkValidationCacheEXT

-- No documentation found for TopLevel "ValidationCacheHeaderVersionEXT"
type ValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT


{-# complete VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: ValidationCacheHeaderVersionEXT #-}


-- No documentation found for Nested "ValidationCacheHeaderVersionEXT" "VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: (a ~ ValidationCacheHeaderVersionEXT) => a
pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT


-- No documentation found for TopLevel "vkCreateValidationCacheEXT"
createValidationCacheEXT :: Device ->  ValidationCacheCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (ValidationCacheEXT)
createValidationCacheEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyValidationCacheEXT"
destroyValidationCacheEXT :: Device ->  ValidationCacheEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyValidationCacheEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetValidationCacheDataEXT"
getNumValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  IO (VkResult, CSize)
getNumValidationCacheDataEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetValidationCacheDataEXT"
getValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  CSize ->  IO (VkResult, ByteString)
getValidationCacheDataEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getValidationCacheDataEXT'.
getAllValidationCacheDataEXT :: Device ->  ValidationCacheEXT ->  IO (ByteString)
getAllValidationCacheDataEXT device' validationCache' =
  snd <$> getNumValidationCacheDataEXT device' validationCache'
    >>= \num -> snd <$> getValidationCacheDataEXT device' validationCache' num



-- No documentation found for TopLevel "vkMergeValidationCachesEXT"
mergeValidationCachesEXT :: Device ->  ValidationCacheEXT ->  Vector ValidationCacheEXT ->  IO ()
mergeValidationCachesEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createValidationCacheEXT' and 'destroyValidationCacheEXT' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withValidationCacheEXT
  :: Device -> ValidationCacheCreateInfoEXT -> Maybe AllocationCallbacks -> (ValidationCacheEXT -> IO a) -> IO a
withValidationCacheEXT device validationCacheCreateInfoEXT allocationCallbacks = bracket
  (createValidationCacheEXT device validationCacheCreateInfoEXT allocationCallbacks)
  (\o -> destroyValidationCacheEXT device o allocationCallbacks)

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_EXTENSION_NAME"
pattern EXT_VALIDATION_CACHE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_VALIDATION_CACHE_EXTENSION_NAME = VK_EXT_VALIDATION_CACHE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_SPEC_VERSION"
pattern EXT_VALIDATION_CACHE_SPEC_VERSION :: Integral a => a
pattern EXT_VALIDATION_CACHE_SPEC_VERSION = VK_EXT_VALIDATION_CACHE_SPEC_VERSION
