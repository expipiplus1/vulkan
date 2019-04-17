{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , VkSemaphoreCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateSemaphore
#endif
  , FN_vkCreateSemaphore
  , PFN_vkCreateSemaphore
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroySemaphore
#endif
  , FN_vkDestroySemaphore
  , PFN_vkDestroySemaphore
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkSemaphore
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkSemaphoreCreateFlags

-- | VkSemaphoreCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkSemaphoreCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkSemaphoreCreateInfo'
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkSemaphoreCreateInfo - Structure specifying parameters of a newly
-- created semaphore
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkSemaphoreCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateSemaphore'
data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
  -- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
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

instance Zero VkSemaphoreCreateInfo where
  zero = VkSemaphoreCreateInfo zero
                               zero
                               zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkCreateSemaphore - Create a new queue semaphore object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the semaphore.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkSemaphoreCreateInfo@ structure which contains information about
--     how the semaphore is to be created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSemaphore@ points to a handle in which the resulting semaphore
--     object is returned.
--
-- = Description
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore', 'VkSemaphoreCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSemaphore" vkCreateSemaphore :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult

#endif
type FN_vkCreateSemaphore = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
type PFN_vkCreateSemaphore = FunPtr FN_vkCreateSemaphore
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkDestroySemaphore - Destroy a semaphore object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the semaphore.
--
-- -   @semaphore@ is the handle of the semaphore to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
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
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @semaphore@
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
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySemaphore" vkDestroySemaphore :: ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroySemaphore = ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySemaphore = FunPtr FN_vkDestroySemaphore
