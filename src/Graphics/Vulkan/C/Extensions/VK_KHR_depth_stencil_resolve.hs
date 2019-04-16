{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkPhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , VkResolveModeFlagBitsKHR(..)
  , pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
  , pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR
  , pattern VK_RESOLVE_MODE_MIN_BIT_KHR
  , pattern VK_RESOLVE_MODE_MAX_BIT_KHR
  , pattern VK_RESOLVE_MODE_NONE_KHR
  , VkResolveModeFlagsKHR
  , VkSubpassDescriptionDepthStencilResolveKHR(..)
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
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
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentReference2KHR(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceDepthStencilResolvePropertiesKHR"
data VkPhysicalDeviceDepthStencilResolvePropertiesKHR = VkPhysicalDeviceDepthStencilResolvePropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedDepthResolveModes"
  vkSupportedDepthResolveModes :: VkResolveModeFlagsKHR
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "supportedStencilResolveModes"
  vkSupportedStencilResolveModes :: VkResolveModeFlagsKHR
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolveNone"
  vkIndependentResolveNone :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "independentResolve"
  vkIndependentResolve :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDepthStencilResolvePropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDepthStencilResolvePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                              <*> peek (ptr `plusPtr` 8)
                                                              <*> peek (ptr `plusPtr` 16)
                                                              <*> peek (ptr `plusPtr` 20)
                                                              <*> peek (ptr `plusPtr` 24)
                                                              <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSupportedDepthResolveModes (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkSupportedStencilResolveModes (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkIndependentResolveNone (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 28) (vkIndependentResolve (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
-- ** VkResolveModeFlagBitsKHR

-- No documentation found for TopLevel "VkResolveModeFlagBitsKHR"
newtype VkResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkResolveModeFlagBitsKHR where
  showsPrec _ VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = showString "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_AVERAGE_BIT_KHR = showString "VK_RESOLVE_MODE_AVERAGE_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_MIN_BIT_KHR = showString "VK_RESOLVE_MODE_MIN_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_MAX_BIT_KHR = showString "VK_RESOLVE_MODE_MAX_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_NONE_KHR = showString "VK_RESOLVE_MODE_NONE_KHR"
  showsPrec p (VkResolveModeFlagBitsKHR x) = showParen (p >= 11) (showString "VkResolveModeFlagBitsKHR " . showsPrec 11 x)

instance Read VkResolveModeFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR", pure VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR)
                             , ("VK_RESOLVE_MODE_AVERAGE_BIT_KHR",     pure VK_RESOLVE_MODE_AVERAGE_BIT_KHR)
                             , ("VK_RESOLVE_MODE_MIN_BIT_KHR",         pure VK_RESOLVE_MODE_MIN_BIT_KHR)
                             , ("VK_RESOLVE_MODE_MAX_BIT_KHR",         pure VK_RESOLVE_MODE_MAX_BIT_KHR)
                             , ("VK_RESOLVE_MODE_NONE_KHR",            pure VK_RESOLVE_MODE_NONE_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkResolveModeFlagBitsKHR")
                        v <- step readPrec
                        pure (VkResolveModeFlagBitsKHR v)
                        )
                    )

-- No documentation found for Nested "VkResolveModeFlagBitsKHR" "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR"
pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkResolveModeFlagBitsKHR" "VK_RESOLVE_MODE_AVERAGE_BIT_KHR"
pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkResolveModeFlagBitsKHR" "VK_RESOLVE_MODE_MIN_BIT_KHR"
pattern VK_RESOLVE_MODE_MIN_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_MIN_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkResolveModeFlagBitsKHR" "VK_RESOLVE_MODE_MAX_BIT_KHR"
pattern VK_RESOLVE_MODE_MAX_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_MAX_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000008

-- No documentation found for Nested "VkResolveModeFlagBitsKHR" "VK_RESOLVE_MODE_NONE_KHR"
pattern VK_RESOLVE_MODE_NONE_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_NONE_KHR = VkResolveModeFlagBitsKHR 0x00000000
-- No documentation found for TopLevel "VkResolveModeFlagsKHR"
type VkResolveModeFlagsKHR = VkResolveModeFlagBitsKHR
-- No documentation found for TopLevel "VkSubpassDescriptionDepthStencilResolveKHR"
data VkSubpassDescriptionDepthStencilResolveKHR = VkSubpassDescriptionDepthStencilResolveKHR
  { -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolveKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolveKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolveKHR" "depthResolveMode"
  vkDepthResolveMode :: VkResolveModeFlagBitsKHR
  , -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolveKHR" "stencilResolveMode"
  vkStencilResolveMode :: VkResolveModeFlagBitsKHR
  , -- No documentation found for Nested "VkSubpassDescriptionDepthStencilResolveKHR" "pDepthStencilResolveAttachment"
  vkPDepthStencilResolveAttachment :: Ptr VkAttachmentReference2KHR
  }
  deriving (Eq, Show)

instance Storable VkSubpassDescriptionDepthStencilResolveKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSubpassDescriptionDepthStencilResolveKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 16) (vkDepthResolveMode (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 20) (vkStencilResolveMode (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 24) (vkPDepthStencilResolveAttachment (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME"
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME = "VK_KHR_depth_stencil_resolve"
-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION"
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR = VkStructureType 1000199000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR"
pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR = VkStructureType 1000199001
