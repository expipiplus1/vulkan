{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPool
  , VkCommandPoolCreateFlagBits(..)
  , pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
  , pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
  , VkCommandPoolCreateFlags
  , VkCommandPoolCreateInfo(..)
  , VkCommandPoolResetFlagBits(..)
  , pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
  , VkCommandPoolResetFlags
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateCommandPool
#endif
  , FN_vkCreateCommandPool
  , PFN_vkCreateCommandPool
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyCommandPool
#endif
  , FN_vkDestroyCommandPool
  , PFN_vkDestroyCommandPool
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkResetCommandPool
#endif
  , FN_vkResetCommandPool
  , PFN_vkResetCommandPool
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkCommandPool_T
-- No documentation found for TopLevel "VkCommandPool"
type VkCommandPool = Ptr VkCommandPool_T
-- ** VkCommandPoolCreateFlagBits

-- No documentation found for TopLevel "VkCommandPoolCreateFlagBits"
newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkCommandPoolCreateFlagBits where
  showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
  showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkCommandPoolCreateFlagBits 0x00000004) = showString "VK_COMMAND_POOL_CREATE_PROTECTED_BIT"
  showsPrec p (VkCommandPoolCreateFlagBits x) = showParen (p >= 11) (showString "VkCommandPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolCreateFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT",            pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT)
                             , ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT", pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_COMMAND_POOL_CREATE_PROTECTED_BIT", pure (VkCommandPoolCreateFlagBits 0x00000004))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolCreateFlagBits")
                        v <- step readPrec
                        pure (VkCommandPoolCreateFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlagBits 0x00000001

-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlagBits 0x00000002
-- No documentation found for TopLevel "VkCommandPoolCreateFlags"
type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits
-- No documentation found for TopLevel "VkCommandPoolCreateInfo"
data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo
  { -- No documentation found for Nested "VkCommandPoolCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCommandPoolCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCommandPoolCreateInfo" "flags"
  vkFlags :: VkCommandPoolCreateFlags
  , -- No documentation found for Nested "VkCommandPoolCreateInfo" "queueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkCommandPoolCreateInfo))

instance Zero VkCommandPoolCreateInfo where
  zero = VkCommandPoolCreateInfo zero
                                 zero
                                 zero
                                 zero
-- ** VkCommandPoolResetFlagBits

-- No documentation found for TopLevel "VkCommandPoolResetFlagBits"
newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkCommandPoolResetFlagBits where
  showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
  showsPrec p (VkCommandPoolResetFlagBits x) = showParen (p >= 11) (showString "VkCommandPoolResetFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolResetFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolResetFlagBits")
                        v <- step readPrec
                        pure (VkCommandPoolResetFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkCommandPoolResetFlagBits" "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT :: VkCommandPoolResetFlagBits
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlagBits 0x00000001
-- No documentation found for TopLevel "VkCommandPoolResetFlags"
type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateCommandPool"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateCommandPool" vkCreateCommandPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult

#endif
type FN_vkCreateCommandPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult
type PFN_vkCreateCommandPool = FunPtr FN_vkCreateCommandPool
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyCommandPool"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyCommandPool" vkDestroyCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyCommandPool = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyCommandPool = FunPtr FN_vkDestroyCommandPool
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkResetCommandPool"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetCommandPool" vkResetCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult

#endif
type FN_vkResetCommandPool = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult
type PFN_vkResetCommandPool = FunPtr FN_vkResetCommandPool
