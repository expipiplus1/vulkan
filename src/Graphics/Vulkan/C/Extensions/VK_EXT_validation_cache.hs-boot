{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT
  , VkValidationCacheCreateFlagsEXT
  , VkValidationCacheCreateInfoEXT
  , VkValidationCacheEXT
  , VkValidationCacheHeaderVersionEXT
  , FN_vkCreateValidationCacheEXT
  , PFN_vkCreateValidationCacheEXT
  , FN_vkDestroyValidationCacheEXT
  , PFN_vkDestroyValidationCacheEXT
  , FN_vkGetValidationCacheDataEXT
  , PFN_vkGetValidationCacheDataEXT
  , FN_vkMergeValidationCachesEXT
  , PFN_vkMergeValidationCachesEXT
  ) where

import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )


data VkShaderModuleValidationCacheCreateInfoEXT

data VkValidationCacheCreateFlagsEXT

data VkValidationCacheCreateInfoEXT

-- | Dummy data to tag the 'Ptr' with
data VkValidationCacheEXT_T
-- No documentation found for TopLevel "VkValidationCacheEXT"
type VkValidationCacheEXT = Ptr VkValidationCacheEXT_T

data VkValidationCacheHeaderVersionEXT

type FN_vkCreateValidationCacheEXT = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult
type PFN_vkCreateValidationCacheEXT = FunPtr FN_vkCreateValidationCacheEXT

type FN_vkDestroyValidationCacheEXT = ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyValidationCacheEXT = FunPtr FN_vkDestroyValidationCacheEXT

type FN_vkGetValidationCacheDataEXT = ("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetValidationCacheDataEXT = FunPtr FN_vkGetValidationCacheDataEXT

type FN_vkMergeValidationCachesEXT = ("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult
type PFN_vkMergeValidationCachesEXT = FunPtr FN_vkMergeValidationCachesEXT
