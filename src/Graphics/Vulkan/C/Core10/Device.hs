{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateFlagBits(..)
  , VkDeviceQueueCreateFlags
  , VkDeviceQueueCreateInfo(..)
  , FN_vkCreateDevice
  , PFN_vkCreateDevice
  , vkCreateDevice
  , FN_vkDestroyDevice
  , PFN_vkDestroyDevice
  , vkDestroyDevice
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
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
  , pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkPhysicalDeviceFeatures(..)
  , VkDevice
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkDeviceCreateFlags

-- No documentation found for TopLevel "VkDeviceCreateFlags"
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDeviceCreateFlags where
  
  showsPrec p (VkDeviceCreateFlags x) = showParen (p >= 11) (showString "VkDeviceCreateFlags " . showsPrec 11 x)

instance Read VkDeviceCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceCreateFlags")
                        v <- step readPrec
                        pure (VkDeviceCreateFlags v)
                        )
                    )



-- No documentation found for TopLevel "VkDeviceCreateInfo"
data VkDeviceCreateInfo = VkDeviceCreateInfo
  { -- No documentation found for Nested "VkDeviceCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceCreateInfo" "flags"
  vkFlags :: VkDeviceCreateFlags
  , -- No documentation found for Nested "VkDeviceCreateInfo" "queueCreateInfoCount"
  vkQueueCreateInfoCount :: Word32
  , -- No documentation found for Nested "VkDeviceCreateInfo" "pQueueCreateInfos"
  vkPQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo
  , -- No documentation found for Nested "VkDeviceCreateInfo" "enabledLayerCount"
  vkEnabledLayerCount :: Word32
  , -- No documentation found for Nested "VkDeviceCreateInfo" "ppEnabledLayerNames"
  vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , -- No documentation found for Nested "VkDeviceCreateInfo" "enabledExtensionCount"
  vkEnabledExtensionCount :: Word32
  , -- No documentation found for Nested "VkDeviceCreateInfo" "ppEnabledExtensionNames"
  vkPPEnabledExtensionNames :: Ptr (Ptr CChar)
  , -- No documentation found for Nested "VkDeviceCreateInfo" "pEnabledFeatures"
  vkPEnabledFeatures :: Ptr VkPhysicalDeviceFeatures
  }
  deriving (Eq, Show)

instance Storable VkDeviceCreateInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkDeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
                                <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueCreateInfoCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPQueueCreateInfos (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPPEnabledLayerNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPPEnabledExtensionNames (poked :: VkDeviceCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPEnabledFeatures (poked :: VkDeviceCreateInfo))

instance Zero VkDeviceCreateInfo where
  zero = VkDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero

-- ** VkDeviceQueueCreateFlagBits

-- No documentation found for TopLevel "VkDeviceQueueCreateFlagBits"
newtype VkDeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDeviceQueueCreateFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDeviceQueueCreateFlagBits 0x00000001) = showString "VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT"
  showsPrec p (VkDeviceQueueCreateFlagBits x) = showParen (p >= 11) (showString "VkDeviceQueueCreateFlagBits " . showsPrec 11 x)

instance Read VkDeviceQueueCreateFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT", pure (VkDeviceQueueCreateFlagBits 0x00000001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceQueueCreateFlagBits")
                        v <- step readPrec
                        pure (VkDeviceQueueCreateFlagBits v)
                        )
                    )



-- No documentation found for TopLevel "VkDeviceQueueCreateFlags"
type VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlagBits

-- No documentation found for TopLevel "VkDeviceQueueCreateInfo"
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo
  { -- No documentation found for Nested "VkDeviceQueueCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "flags"
  vkFlags :: VkDeviceQueueCreateFlags
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "queueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "queueCount"
  vkQueueCount :: Word32
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "pQueuePriorities"
  vkPQueuePriorities :: Ptr CFloat
  }
  deriving (Eq, Show)

instance Storable VkDeviceQueueCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceQueueCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueueCount (poked :: VkDeviceQueueCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPQueuePriorities (poked :: VkDeviceQueueCreateInfo))

instance Zero VkDeviceQueueCreateInfo where
  zero = VkDeviceQueueCreateInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- No documentation found for TopLevel "vkCreateDevice"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDevice" vkCreateDevice :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
#else
vkCreateDevice :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
vkCreateDevice deviceCmds = mkVkCreateDevice (pVkCreateDevice deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDevice
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult)
#endif

type FN_vkCreateDevice = ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
type PFN_vkCreateDevice = FunPtr FN_vkCreateDevice

-- No documentation found for TopLevel "vkDestroyDevice"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDevice" vkDestroyDevice :: ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDevice :: DeviceCmds -> ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDevice deviceCmds = mkVkDestroyDevice (pVkDestroyDevice deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDevice
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDevice = ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDevice = FunPtr FN_vkDestroyDevice
