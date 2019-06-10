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
  , FN_vkCreateEvent
  , PFN_vkCreateEvent
  , vkCreateEvent
  , FN_vkDestroyEvent
  , PFN_vkDestroyEvent
  , vkDestroyEvent
  , FN_vkGetEventStatus
  , PFN_vkGetEventStatus
  , vkGetEventStatus
  , FN_vkResetEvent
  , PFN_vkResetEvent
  , vkResetEvent
  , FN_vkSetEvent
  , PFN_vkSetEvent
  , vkSetEvent
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
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
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
  zero = VkEventCreateInfo VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
                           zero
                           zero

-- No documentation found for TopLevel "vkCreateEvent"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateEvent" vkCreateEvent :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
#else
vkCreateEvent :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
vkCreateEvent deviceCmds = mkVkCreateEvent (pVkCreateEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateEvent
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult)
#endif

type FN_vkCreateEvent = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
type PFN_vkCreateEvent = FunPtr FN_vkCreateEvent

-- No documentation found for TopLevel "vkDestroyEvent"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyEvent" vkDestroyEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyEvent :: DeviceCmds -> ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyEvent deviceCmds = mkVkDestroyEvent (pVkDestroyEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyEvent
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyEvent = FunPtr FN_vkDestroyEvent

-- No documentation found for TopLevel "vkGetEventStatus"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetEventStatus" vkGetEventStatus :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
#else
vkGetEventStatus :: DeviceCmds -> ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
vkGetEventStatus deviceCmds = mkVkGetEventStatus (pVkGetEventStatus deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetEventStatus
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
#endif

type FN_vkGetEventStatus = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkGetEventStatus = FunPtr FN_vkGetEventStatus

-- No documentation found for TopLevel "vkResetEvent"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetEvent" vkResetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
#else
vkResetEvent :: DeviceCmds -> ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
vkResetEvent deviceCmds = mkVkResetEvent (pVkResetEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetEvent
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
#endif

type FN_vkResetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkResetEvent = FunPtr FN_vkResetEvent

-- No documentation found for TopLevel "vkSetEvent"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetEvent" vkSetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
#else
vkSetEvent :: DeviceCmds -> ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
vkSetEvent deviceCmds = mkVkSetEvent (pVkSetEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetEvent
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
#endif

type FN_vkSetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkSetEvent = FunPtr FN_vkSetEvent
