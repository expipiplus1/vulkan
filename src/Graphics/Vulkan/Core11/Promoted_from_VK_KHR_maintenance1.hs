{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , vkTrimCommandPool
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Storable
  ( Storable(..)
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


import Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  , VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  )


-- ** VkCommandPoolTrimFlags

-- | 
newtype VkCommandPoolTrimFlags = VkCommandPoolTrimFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- | Nothing
pattern VK_ERROR_OUT_OF_POOL_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_POOL_MEMORY = VkResult (-1000069000)
-- | Just "The 3D image can be viewed as a 2D or 2D array image"
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000020
-- | Just "Format can be used as the source image of image transfer commands"
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT = VkFormatFeatureFlagBits 0x00004000
-- | Just "Format can be used as the destination image of image transfer commands"
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT = VkFormatFeatureFlagBits 0x00008000
-- | 
foreign import ccall "vkTrimCommandPool" vkTrimCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
