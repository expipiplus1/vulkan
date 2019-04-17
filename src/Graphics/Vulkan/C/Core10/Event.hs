{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Event
  ( VkEvent
  , VkEventCreateFlags(..)
  , VkEventCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateEvent
#endif
  , FN_vkCreateEvent
  , PFN_vkCreateEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyEvent
#endif
  , FN_vkDestroyEvent
  , PFN_vkDestroyEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetEventStatus
#endif
  , FN_vkGetEventStatus
  , PFN_vkGetEventStatus
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkResetEvent
#endif
  , FN_vkResetEvent
  , PFN_vkResetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkSetEvent
#endif
  , FN_vkSetEvent
  , PFN_vkSetEvent
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkEvent_T
-- No documentation found for TopLevel "VkEvent"
type VkEvent = Ptr VkEvent_T
-- ** VkEventCreateFlags

-- No documentation found for TopLevel "VkEventCreateFlags"
newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkEventCreateFlags where
  
  showsPrec p (VkEventCreateFlags x) = showParen (p >= 11) (showString "VkEventCreateFlags " . showsPrec 11 x)

instance Read VkEventCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkEventCreateFlags")
                        v <- step readPrec
                        pure (VkEventCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkEventCreateInfo"
data VkEventCreateInfo = VkEventCreateInfo
  { -- No documentation found for Nested "VkEventCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkEventCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkEventCreateInfo" "flags"
  vkFlags :: VkEventCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkEventCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkEventCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkEventCreateInfo))

instance Zero VkEventCreateInfo where
  zero = VkEventCreateInfo zero
                           zero
                           zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateEvent"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateEvent" vkCreateEvent :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult

#endif
type FN_vkCreateEvent = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
type PFN_vkCreateEvent = FunPtr FN_vkCreateEvent
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyEvent"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyEvent" vkDestroyEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyEvent = FunPtr FN_vkDestroyEvent
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetEventStatus"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetEventStatus" vkGetEventStatus :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult

#endif
type FN_vkGetEventStatus = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkGetEventStatus = FunPtr FN_vkGetEventStatus
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkResetEvent"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetEvent" vkResetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult

#endif
type FN_vkResetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkResetEvent = FunPtr FN_vkResetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkSetEvent"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetEvent" vkSetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult

#endif
type FN_vkSetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkSetEvent = FunPtr FN_vkSetEvent
