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
  , FN_vkCreateSemaphore
  , PFN_vkCreateSemaphore
  , vkCreateSemaphore
  , FN_vkDestroySemaphore
  , PFN_vkDestroySemaphore
  , vkDestroySemaphore
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
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkSemaphore
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkSemaphoreCreateFlags

-- No documentation found for TopLevel "VkSemaphoreCreateFlags"
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



-- No documentation found for TopLevel "VkSemaphoreCreateInfo"
data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo
  { -- No documentation found for Nested "VkSemaphoreCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSemaphoreCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSemaphoreCreateInfo" "flags"
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
  zero = VkSemaphoreCreateInfo VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
                               zero
                               zero

-- No documentation found for TopLevel "vkCreateSemaphore"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSemaphore" vkCreateSemaphore :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
#else
vkCreateSemaphore :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
vkCreateSemaphore deviceCmds = mkVkCreateSemaphore (pVkCreateSemaphore deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSemaphore
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult)
#endif

type FN_vkCreateSemaphore = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
type PFN_vkCreateSemaphore = FunPtr FN_vkCreateSemaphore

-- No documentation found for TopLevel "vkDestroySemaphore"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySemaphore" vkDestroySemaphore :: ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroySemaphore :: DeviceCmds -> ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroySemaphore deviceCmds = mkVkDestroySemaphore (pVkDestroySemaphore deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySemaphore
  :: FunPtr (("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroySemaphore = ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySemaphore = FunPtr FN_vkDestroySemaphore
