{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits(..)
  , pattern VK_FENCE_CREATE_SIGNALED_BIT
  , VkFenceCreateFlags
  , VkFenceCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateFence
#endif
  , FN_vkCreateFence
  , PFN_vkCreateFence
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyFence
#endif
  , FN_vkDestroyFence
  , PFN_vkDestroyFence
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetFenceStatus
#endif
  , FN_vkGetFenceStatus
  , PFN_vkGetFenceStatus
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkResetFences
#endif
  , FN_vkResetFences
  , PFN_vkResetFences
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkWaitForFences
#endif
  , FN_vkWaitForFences
  , PFN_vkWaitForFences
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  , Word64
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkFenceCreateFlagBits

-- No documentation found for TopLevel "VkFenceCreateFlagBits"
newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkFenceCreateFlagBits where
  showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT = showString "VK_FENCE_CREATE_SIGNALED_BIT"
  showsPrec p (VkFenceCreateFlagBits x) = showParen (p >= 11) (showString "VkFenceCreateFlagBits " . showsPrec 11 x)

instance Read VkFenceCreateFlagBits where
  readPrec = parens ( choose [ ("VK_FENCE_CREATE_SIGNALED_BIT", pure VK_FENCE_CREATE_SIGNALED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFenceCreateFlagBits")
                        v <- step readPrec
                        pure (VkFenceCreateFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkFenceCreateFlagBits" "VK_FENCE_CREATE_SIGNALED_BIT"
pattern VK_FENCE_CREATE_SIGNALED_BIT :: VkFenceCreateFlagBits
pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x00000001
-- No documentation found for TopLevel "VkFenceCreateFlags"
type VkFenceCreateFlags = VkFenceCreateFlagBits
-- No documentation found for TopLevel "VkFenceCreateInfo"
data VkFenceCreateInfo = VkFenceCreateInfo
  { -- No documentation found for Nested "VkFenceCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFenceCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFenceCreateInfo" "flags"
  vkFlags :: VkFenceCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFenceCreateInfo))

instance Zero VkFenceCreateInfo where
  zero = VkFenceCreateInfo zero
                           zero
                           zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateFence"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateFence" vkCreateFence :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult

#endif
type FN_vkCreateFence = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkCreateFence = FunPtr FN_vkCreateFence
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyFence"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyFence" vkDestroyFence :: ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyFence = ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyFence = FunPtr FN_vkDestroyFence
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetFenceStatus"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetFenceStatus" vkGetFenceStatus :: ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult

#endif
type FN_vkGetFenceStatus = ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkGetFenceStatus = FunPtr FN_vkGetFenceStatus
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkResetFences"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetFences" vkResetFences :: ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult

#endif
type FN_vkResetFences = ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult
type PFN_vkResetFences = FunPtr FN_vkResetFences
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkWaitForFences"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkWaitForFences" vkWaitForFences :: ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult

#endif
type FN_vkWaitForFences = ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult
type PFN_vkWaitForFences = FunPtr FN_vkWaitForFences
