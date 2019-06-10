{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_atomic_int64
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , 
#endif
  pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  , pattern KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  , pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceShaderAtomicInt64FeaturesKHR"
data PhysicalDeviceShaderAtomicInt64FeaturesKHR = PhysicalDeviceShaderAtomicInt64FeaturesKHR
  { -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "shaderBufferInt64Atomics"
  shaderBufferInt64Atomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "shaderSharedInt64Atomics"
  shaderSharedInt64Atomics :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceShaderAtomicInt64FeaturesKHR where
  zero = PhysicalDeviceShaderAtomicInt64FeaturesKHR Nothing
                                                    False
                                                    False

#endif

-- No documentation found for TopLevel "VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME"
pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME = VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION"
pattern KHR_SHADER_ATOMIC_INT64_SPEC_VERSION :: Integral a => a
pattern KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
