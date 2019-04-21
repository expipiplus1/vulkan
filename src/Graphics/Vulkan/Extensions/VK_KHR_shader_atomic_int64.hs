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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
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



-- | VkPhysicalDeviceShaderAtomicInt64FeaturesKHR - Structure describing
-- features supported by VK_KHR_shader_atomic_int64
--
-- = Description
--
-- Unresolved directive in VkPhysicalDeviceShaderAtomicInt64FeaturesKHR.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceShaderAtomicInt64FeaturesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceShaderAtomicInt64FeaturesKHR = PhysicalDeviceShaderAtomicInt64FeaturesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "shaderBufferInt64Atomics"
  shaderBufferInt64Atomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceShaderAtomicInt64FeaturesKHR" "shaderSharedInt64Atomics"
  shaderSharedInt64Atomics :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceShaderAtomicInt64FeaturesKHR' and
-- marshal a 'PhysicalDeviceShaderAtomicInt64FeaturesKHR' into it. The 'VkPhysicalDeviceShaderAtomicInt64FeaturesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR :: PhysicalDeviceShaderAtomicInt64FeaturesKHR -> (VkPhysicalDeviceShaderAtomicInt64FeaturesKHR -> IO a) -> IO a
withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceShaderAtomicInt64FeaturesKHR)) (\pPNext -> cont (VkPhysicalDeviceShaderAtomicInt64FeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR pPNext (boolToBool32 (shaderBufferInt64Atomics (marshalled :: PhysicalDeviceShaderAtomicInt64FeaturesKHR))) (boolToBool32 (shaderSharedInt64Atomics (marshalled :: PhysicalDeviceShaderAtomicInt64FeaturesKHR)))))

-- | A function to read a 'VkPhysicalDeviceShaderAtomicInt64FeaturesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceShaderAtomicInt64FeaturesKHR'.
fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR -> IO PhysicalDeviceShaderAtomicInt64FeaturesKHR
fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR c = PhysicalDeviceShaderAtomicInt64FeaturesKHR <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkShaderBufferInt64Atomics (c :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkShaderSharedInt64Atomics (c :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)))

instance Zero PhysicalDeviceShaderAtomicInt64FeaturesKHR where
  zero = PhysicalDeviceShaderAtomicInt64FeaturesKHR Nothing
                                                    False
                                                    False

