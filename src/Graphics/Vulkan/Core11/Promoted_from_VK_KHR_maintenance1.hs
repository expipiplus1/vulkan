{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , vkTrimCommandPool
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Storable
  ( Storable(..)
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


import Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkDevice
  )


-- ** VkCommandPoolTrimFlags

-- | VkCommandPoolTrimFlags - Reserved for future use
--
-- = Description
--
-- @VkCommandPoolTrimFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'vkTrimCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance1.vkTrimCommandPoolKHR'
newtype VkCommandPoolTrimFlags = VkCommandPoolTrimFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCommandPoolTrimFlags where
  
  showsPrec p (VkCommandPoolTrimFlags x) = showParen (p >= 11) (showString "VkCommandPoolTrimFlags " . showsPrec 11 x)

instance Read VkCommandPoolTrimFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolTrimFlags")
                        v <- step readPrec
                        pure (VkCommandPoolTrimFlags v)
                        )
                    )


-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_POOL_MEMORY"
pattern VK_ERROR_OUT_OF_POOL_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_POOL_MEMORY = VkResult (-1000069000)
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000020
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT"
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT = VkFormatFeatureFlagBits 0x00004000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_TRANSFER_DST_BIT"
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT = VkFormatFeatureFlagBits 0x00008000
-- | vkTrimCommandPool - Trim a command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool to trim.
--
-- -   @flags@ is reserved for future use.
--
-- = Description
--
-- Trimming a command pool recycles unused memory from the command pool
-- back to the system. Command buffers allocated from the pool are not
-- affected by the command.
--
-- __Note__
--
-- This command provides applications with some control over the internal
-- memory allocations used by command pools.
--
-- Unused memory normally arises from command buffers that have been
-- recorded and later reset, such that they are no longer using the memory.
-- On reset, a command buffer can return memory to its command pool, but
-- the only way to release memory from a command pool to the system
-- requires calling
-- 'Graphics.Vulkan.Core10.CommandPool.vkResetCommandPool', which cannot be
-- executed while any command buffers from that pool are still in use.
-- Subsequent recording operations into command buffers will re-use this
-- memory but since total memory requirements fluctuate over time, unused
-- memory can accumulate.
--
-- In this situation, trimming a command pool /may/ be useful to return
-- unused memory back to the system, returning the total outstanding memory
-- allocated by the pool back to a more “average” value.
--
-- Implementations utilize many internal allocation strategies that make it
-- impossible to guarantee that all unused memory is released back to the
-- system. For instance, an implementation of a command pool /may/ involve
-- allocating memory in bulk from the system and sub-allocating from that
-- memory. In such an implementation any live command buffer that holds a
-- reference to a bulk allocation would prevent that allocation from being
-- freed, even if only a small proportion of the bulk allocation is in use.
--
-- In most cases trimming will result in a reduction in allocated but
-- unused memory, but it does not guarantee the “ideal” behaviour.
--
-- Trimming /may/ be an expensive operation, and /should/ not be called
-- frequently. Trimming /should/ be treated as a way to relieve memory
-- pressure after application-known points when there exists enough unused
-- memory that the cost of trimming is “worth” it.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @commandPool@ /must/ be a valid @VkCommandPool@ handle
--
-- -   @flags@ /must/ be @0@
--
-- -   @commandPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandPool.VkCommandPool',
-- 'VkCommandPoolTrimFlags',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkTrimCommandPool" vkTrimCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
