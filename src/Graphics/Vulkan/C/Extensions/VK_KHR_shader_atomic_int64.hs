{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( VkPhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  , pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- | VkPhysicalDeviceShaderAtomicInt64FeaturesKHR - Structure describing
-- features supported by VK_KHR_shader_atomic_int64
--
-- = Description
--
-- Unresolved directive in VkPhysicalDeviceShaderAtomicInt64FeaturesKHR.txt
-- -
-- include::..\/validity\/structs\/VkPhysicalDeviceShaderAtomicInt64FeaturesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceShaderAtomicInt64FeaturesKHR = VkPhysicalDeviceShaderAtomicInt64FeaturesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @shaderBufferInt64Atomics@ indicates whether shaders /can/ support
  -- 64-bit unsigned and signed integer atomic operations on buffers.
  vkShaderBufferInt64Atomics :: VkBool32
  , -- | @shaderSharedInt64Atomics@ indicates whether shaders /can/ support
  -- 64-bit unsigned and signed integer atomic operations on shared memory.
  vkShaderSharedInt64Atomics :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShaderAtomicInt64FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShaderAtomicInt64FeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkShaderBufferInt64Atomics (poked :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR))
                *> poke (ptr `plusPtr` 20) (vkShaderSharedInt64Atomics (poked :: VkPhysicalDeviceShaderAtomicInt64FeaturesKHR))

instance Zero VkPhysicalDeviceShaderAtomicInt64FeaturesKHR where
  zero = VkPhysicalDeviceShaderAtomicInt64FeaturesKHR zero
                                                      zero
                                                      zero
                                                      zero
-- No documentation found for TopLevel "VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME"
pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME = "VK_KHR_shader_atomic_int64"
-- No documentation found for TopLevel "VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION"
pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR = VkStructureType 1000180000
