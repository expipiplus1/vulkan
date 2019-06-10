{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , FN_vkTrimCommandPool
  , PFN_vkTrimCommandPool
  , vkTrimCommandPool
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( FunPtr
  )
import Foreign.Storable
  ( Storable(..)
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


import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPool
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkCommandPoolTrimFlags

-- No documentation found for TopLevel "VkCommandPoolTrimFlags"
newtype VkCommandPoolTrimFlags = VkCommandPoolTrimFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "vkTrimCommandPool"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkTrimCommandPool" vkTrimCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
#else
vkTrimCommandPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
vkTrimCommandPool deviceCmds = mkVkTrimCommandPool (pVkTrimCommandPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkTrimCommandPool
  :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()) -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ())
#endif

type FN_vkTrimCommandPool = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
type PFN_vkTrimCommandPool = FunPtr FN_vkTrimCommandPool

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_POOL_MEMORY"
pattern VK_ERROR_OUT_OF_POOL_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_POOL_MEMORY = VkResult (-1000069000)

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_TRANSFER_DST_BIT"
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT = VkFormatFeatureFlagBits 0x00008000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT"
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT = VkFormatFeatureFlagBits 0x00004000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000020
