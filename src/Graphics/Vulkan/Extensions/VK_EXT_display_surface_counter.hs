{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagBitsEXT(..)
  , pattern VK_SURFACE_COUNTER_VBLANK_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
  , vkGetPhysicalDeviceSurfaceCapabilities2EXT
  , VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagsEXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageUsageFlags
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkExtent2D(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkCompositeAlphaFlagsKHR
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceTransformFlagsKHR
  , VkSurfaceKHR
  )


-- ** VkSurfaceCounterFlagBitsEXT

-- | 
newtype VkSurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSurfaceCounterFlagBitsEXT where
  showsPrec _ VK_SURFACE_COUNTER_VBLANK_EXT = showString "VK_SURFACE_COUNTER_VBLANK_EXT"
  showsPrec p (VkSurfaceCounterFlagBitsEXT x) = showParen (p >= 11) (showString "VkSurfaceCounterFlagBitsEXT " . showsPrec 11 x)

instance Read VkSurfaceCounterFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_SURFACE_COUNTER_VBLANK_EXT", pure VK_SURFACE_COUNTER_VBLANK_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSurfaceCounterFlagBitsEXT")
                        v <- step readPrec
                        pure (VkSurfaceCounterFlagBitsEXT v)
                        )
                    )

-- | 
pattern VK_SURFACE_COUNTER_VBLANK_EXT :: VkSurfaceCounterFlagBitsEXT
pattern VK_SURFACE_COUNTER_VBLANK_EXT = VkSurfaceCounterFlagBitsEXT 0x00000001
-- | Nothing
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT = VkStructureType 1000090000
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
-- | 
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilities2EXT" vkGetPhysicalDeviceSurfaceCapabilities2EXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult
-- | TODO: Struct comments
data VkSurfaceCapabilities2EXT = VkSurfaceCapabilities2EXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMinImageCount :: Word32
  , vkMaxImageCount :: Word32
  , vkCurrentExtent :: VkExtent2D
  , vkMinImageExtent :: VkExtent2D
  , vkMaxImageExtent :: VkExtent2D
  , vkMaxImageArrayLayers :: Word32
  , vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR
  , vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR
  , vkSupportedUsageFlags :: VkImageUsageFlags
  , vkSupportedSurfaceCounters :: VkSurfaceCounterFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkSurfaceCapabilities2EXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSurfaceCapabilities2EXT <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
                                       <*> peek (ptr `plusPtr` 40)
                                       <*> peek (ptr `plusPtr` 48)
                                       <*> peek (ptr `plusPtr` 52)
                                       <*> peek (ptr `plusPtr` 56)
                                       <*> peek (ptr `plusPtr` 60)
                                       <*> peek (ptr `plusPtr` 64)
                                       <*> peek (ptr `plusPtr` 68)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 16) (vkMinImageCount (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 20) (vkMaxImageCount (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 24) (vkCurrentExtent (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 32) (vkMinImageExtent (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 40) (vkMaxImageExtent (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 48) (vkMaxImageArrayLayers (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 52) (vkSupportedTransforms (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 56) (vkCurrentTransform (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 60) (vkSupportedCompositeAlpha (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 64) (vkSupportedUsageFlags (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 68) (vkSupportedSurfaceCounters (poked :: VkSurfaceCapabilities2EXT))
type VkSurfaceCounterFlagsEXT = VkSurfaceCounterFlagBitsEXT
