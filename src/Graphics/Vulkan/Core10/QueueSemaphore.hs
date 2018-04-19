{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , vkCreateSemaphore
  , vkDestroySemaphore
  , VkSemaphoreCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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
  ( VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Queue
  ( VkSemaphore
  )


-- ** VkSemaphoreCreateFlags

-- | VkSemaphoreCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkSemaphoreCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkSemaphoreCreateInfo'
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSemaphoreCreateFlags where
  
  showsPrec p (VkSemaphoreCreateFlags x) = showParen (p >= 11) (showString "VkSemaphoreCreateFlags " . showsPrec 11 x)

instance Read VkSemaphoreCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSemaphoreCreateFlags")
                        v <- step readPrec
                        pure (VkSemaphoreCreateFlags v)
                        )
                    )


-- | vkCreateSemaphore - Create a new queue semaphore object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the semaphore.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkSemaphoreCreateInfo@ structure which contains information about
--     how the semaphore is to be created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pSemaphore@ points to a handle in which the resulting semaphore
--     object is returned.
--
-- = Description
-- #_description#
--
-- When created, the semaphore is in the unsignaled state.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkSemaphoreCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSemaphore@ /must/ be a valid pointer to a @VkSemaphore@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkSemaphore', 'VkSemaphoreCreateInfo'
foreign import ccall "vkCreateSemaphore" vkCreateSemaphore :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
-- | vkDestroySemaphore - Destroy a semaphore object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the semaphore.
--
-- -   @semaphore@ is the handle of the semaphore to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   All submitted batches that refer to @semaphore@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @semaphore@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @semaphore@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @semaphore@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @semaphore@
--     /must/ be a valid @VkSemaphore@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @semaphore@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkSemaphore'
foreign import ccall "vkDestroySemaphore" vkDestroySemaphore :: ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkSemaphoreCreateInfo - Structure specifying parameters of a newly
-- created semaphore
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkExportSemaphoreWin32HandleInfoKHR'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkSemaphoreCreateFlags', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateSemaphore'
data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo
  { -- No documentation found for Nested "VkSemaphoreCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSemaphoreCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSemaphoreCreateInfo" "vkFlags"
  vkFlags :: VkSemaphoreCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSemaphoreCreateInfo))
