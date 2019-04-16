{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_atomic_int64
  ( withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR
  , fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR
  , PhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
  , pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( VkPhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  , pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceShaderAtomicInt64FeaturesKHR"
data PhysicalDeviceShaderAtomicInt64FeaturesKHR = PhysicalDeviceShaderAtomicInt64FeaturesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "shaderBufferInt64Atomics"
  vkShaderBufferInt64Atomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "shaderSharedInt64Atomics"
  vkShaderSharedInt64Atomics :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR :: PhysicalDeviceShaderAtomicInt64FeaturesKHR -> (VkPhysicalDeviceShaderAtomicInt64FeaturesKHR -> IO a) -> IO a
withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceShaderAtomicInt64FeaturesKHR)) (\pPNext -> cont (VkPhysicalDeviceShaderAtomicInt64FeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR pPNext (boolToBool32 (vkShaderBufferInt64Atomics (from :: PhysicalDeviceShaderAtomicInt64FeaturesKHR))) (boolToBool32 (vkShaderSharedInt64Atomics (from :: PhysicalDeviceShaderAtomicInt64FeaturesKHR)))))
fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR -> IO PhysicalDeviceShaderAtomicInt64FeaturesKHR
fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR c = PhysicalDeviceShaderAtomicInt64FeaturesKHR <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkShaderBufferInt64Atomics (c :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkShaderSharedInt64Atomics (c :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)))
