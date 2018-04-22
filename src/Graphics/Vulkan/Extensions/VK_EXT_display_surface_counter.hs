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
  ( VkSurfaceTransformFlagBitsKHR(..)
  , VkCompositeAlphaFlagsKHR
  , VkSurfaceKHR
  , VkSurfaceTransformFlagsKHR
  )


-- ** VkSurfaceCounterFlagBitsEXT

-- | VkSurfaceCounterFlagBitsEXT - Surface-relative counter types
--
-- = See Also
--
-- 'VkSurfaceCounterFlagsEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkGetSwapchainCounterEXT'
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

-- | @VK_SURFACE_COUNTER_VBLANK_EXT@ specifies a counter incrementing once
-- every time a vertical blanking period occurs on the display associated
-- with the surface.
pattern VK_SURFACE_COUNTER_VBLANK_EXT :: VkSurfaceCounterFlagBitsEXT
pattern VK_SURFACE_COUNTER_VBLANK_EXT = VkSurfaceCounterFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT = VkStructureType 1000090000
-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION"
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME"
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = "VK_EXT_display_surface_counter"
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
-- | vkGetPhysicalDeviceSurfaceCapabilities2EXT - Query surface capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceCapabilities@ is a pointer to an instance of the
--     'VkSurfaceCapabilities2EXT' structure in which the capabilities are
--     returned.
--
-- = Description
--
-- @vkGetPhysicalDeviceSurfaceCapabilities2EXT@ behaves similarly to
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- with the ability to return extended information by adding extension
-- structures to the @pNext@ chain of its @pSurfaceCapabilities@ parameter.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @surface@ /must/ be a valid @VkSurfaceKHR@ handle
--
-- -   @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     @VkSurfaceCapabilities2EXT@ structure
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfaceCapabilities2EXT" vkGetPhysicalDeviceSurfaceCapabilities2EXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult
-- | VkSurfaceCapabilities2EXT - Structure describing capabilities of a
-- surface
--
-- = Members
--
-- All members of @VkSurfaceCapabilities2EXT@ are identical to the
-- corresponding members of
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
-- where one exists. The remaining members are:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to an extension-specific structure.
--
-- -   @supportedSurfaceCounters@ is a bitmask of
--     'VkSurfaceCounterFlagBitsEXT' indicating the supported surface
--     counter types.
--
-- == Valid Usage
--
-- -   @supportedSurfaceCounters@ /must/ not include
--     @VK_SURFACE_COUNTER_VBLANK_EXT@ unless the surface queried is a
--     [display
--     surface](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#wsi-display-surfaces).
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkCompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkSurfaceCounterFlagsEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceTransformFlagsKHR',
-- 'vkGetPhysicalDeviceSurfaceCapabilities2EXT'
data VkSurfaceCapabilities2EXT = VkSurfaceCapabilities2EXT
  { -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "minImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageCount"
  vkMaxImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "currentExtent"
  vkCurrentExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "minImageExtent"
  vkMinImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageExtent"
  vkMaxImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "maxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedTransforms"
  vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "currentTransform"
  vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedCompositeAlpha"
  vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedUsageFlags"
  vkSupportedUsageFlags :: VkImageUsageFlags
  , -- No documentation found for Nested "VkSurfaceCapabilities2EXT" "supportedSurfaceCounters"
  vkSupportedSurfaceCounters :: VkSurfaceCounterFlagsEXT
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceCapabilities2EXT))
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
-- | VkSurfaceCounterFlagsEXT - Bitmask of VkSurfaceCounterFlagBitsEXT
--
-- = Description
--
-- @VkSurfaceCounterFlagsEXT@ is a bitmask type for setting a mask of zero
-- or more 'VkSurfaceCounterFlagBitsEXT'.
--
-- = See Also
--
-- 'VkSurfaceCapabilities2EXT', 'VkSurfaceCounterFlagBitsEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.VkSwapchainCounterCreateInfoEXT'
type VkSurfaceCounterFlagsEXT = VkSurfaceCounterFlagBitsEXT
