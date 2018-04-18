{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceQueueCreateFlagBits(..)
  , vkCreateDevice
  , vkDestroyDevice
  , VkDeviceQueueCreateInfo(..)
  , VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateFlags
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
  ( VkPhysicalDeviceFeatures(..)
  , VkDevice
  , VkAllocationCallbacks(..)
  , VkPhysicalDevice
  )


-- ** VkDeviceCreateFlags

-- | 
newtype VkDeviceCreateFlags = VkDeviceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- ** VkDeviceQueueCreateFlagBits

-- | 
newtype VkDeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | 
foreign import ccall "vkCreateDevice" vkCreateDevice :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
-- | 
foreign import ccall "vkDestroyDevice" vkDestroyDevice :: ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | TODO: Struct comments
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkDeviceQueueCreateFlags
  , vkQueueFamilyIndex :: Word32
  , vkQueueCount :: Word32
  , vkPQueuePriorities :: Ptr CFloat
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
-- | TODO: Struct comments
data VkDeviceCreateInfo = VkDeviceCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkDeviceCreateFlags
  , vkQueueCreateInfoCount :: Word32
  , vkPQueueCreateInfos :: Ptr VkDeviceQueueCreateInfo
  , vkEnabledLayerCount :: Word32
  , vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , vkEnabledExtensionCount :: Word32
  , vkPPEnabledExtensionNames :: Ptr (Ptr CChar)
  , vkPEnabledFeatures :: Ptr VkPhysicalDeviceFeatures
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
type VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlagBits
