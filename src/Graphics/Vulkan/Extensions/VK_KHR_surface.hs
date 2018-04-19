{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  , VkPresentModeKHR(..)
  , pattern VK_PRESENT_MODE_IMMEDIATE_KHR
  , pattern VK_PRESENT_MODE_MAILBOX_KHR
  , pattern VK_PRESENT_MODE_FIFO_KHR
  , pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR
  , VkCompositeAlphaFlagBitsKHR(..)
  , pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , VkSurfaceTransformFlagBitsKHR(..)
  , pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_OBJECT_TYPE_SURFACE_KHR
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR
  , VkSurfaceKHR
  , vkDestroySurfaceKHR
  , vkGetPhysicalDeviceSurfaceSupportKHR
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , vkGetPhysicalDeviceSurfaceFormatsKHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkCompositeAlphaFlagsKHR
  , VkSurfaceTransformFlagsKHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
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
  ( VkFormat(..)
  , VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageUsageFlags
  , VkPhysicalDevice
  , VkAllocationCallbacks(..)
  , VkInstance
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkExtent2D(..)
  )


-- ** VkColorSpaceKHR

-- | VkColorSpaceKHR - supported color space of the presentation engine
--
-- = See Also
-- #_see_also#
--
-- 'VkSurfaceFormatKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkColorSpaceKHR where
  showsPrec _ VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = showString "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkColorSpaceKHR 1000104001) = showString "VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104002) = showString "VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104003) = showString "VK_COLOR_SPACE_DCI_P3_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104004) = showString "VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104005) = showString "VK_COLOR_SPACE_BT709_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104006) = showString "VK_COLOR_SPACE_BT709_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104007) = showString "VK_COLOR_SPACE_BT2020_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104008) = showString "VK_COLOR_SPACE_HDR10_ST2084_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104009) = showString "VK_COLOR_SPACE_DOLBYVISION_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104010) = showString "VK_COLOR_SPACE_HDR10_HLG_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104011) = showString "VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104012) = showString "VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104013) = showString "VK_COLOR_SPACE_PASS_THROUGH_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104014) = showString "VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT"
  showsPrec p (VkColorSpaceKHR x) = showParen (p >= 11) (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
  readPrec = parens ( choose [ ("VK_COLOR_SPACE_SRGB_NONLINEAR_KHR", pure VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT",    pure (VkColorSpaceKHR 1000104001))
                             , ("VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT",    pure (VkColorSpaceKHR 1000104002))
                             , ("VK_COLOR_SPACE_DCI_P3_LINEAR_EXT",           pure (VkColorSpaceKHR 1000104003))
                             , ("VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT",        pure (VkColorSpaceKHR 1000104004))
                             , ("VK_COLOR_SPACE_BT709_LINEAR_EXT",            pure (VkColorSpaceKHR 1000104005))
                             , ("VK_COLOR_SPACE_BT709_NONLINEAR_EXT",         pure (VkColorSpaceKHR 1000104006))
                             , ("VK_COLOR_SPACE_BT2020_LINEAR_EXT",           pure (VkColorSpaceKHR 1000104007))
                             , ("VK_COLOR_SPACE_HDR10_ST2084_EXT",            pure (VkColorSpaceKHR 1000104008))
                             , ("VK_COLOR_SPACE_DOLBYVISION_EXT",             pure (VkColorSpaceKHR 1000104009))
                             , ("VK_COLOR_SPACE_HDR10_HLG_EXT",               pure (VkColorSpaceKHR 1000104010))
                             , ("VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT",         pure (VkColorSpaceKHR 1000104011))
                             , ("VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT",      pure (VkColorSpaceKHR 1000104012))
                             , ("VK_COLOR_SPACE_PASS_THROUGH_EXT",            pure (VkColorSpaceKHR 1000104013))
                             , ("VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT", pure (VkColorSpaceKHR 1000104014))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkColorSpaceKHR")
                        v <- step readPrec
                        pure (VkColorSpaceKHR v)
                        )
                    )

-- | @VK_COLOR_SPACE_SRGB_NONLINEAR_KHR@ specifies support for the sRGB color
-- space.
pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0
-- ** VkPresentModeKHR

-- | VkPresentModeKHR - presentation mode supported for a surface
--
-- = Description
-- #_description#
--
-- -   @VK_PRESENT_MODE_IMMEDIATE_KHR@ specifies that the presentation
--     engine does not wait for a vertical blanking period to update the
--     current image, meaning this mode /may/ result in visible tearing. No
--     internal queuing of presentation requests is needed, as the requests
--     are applied immediately.
--
-- -   @VK_PRESENT_MODE_MAILBOX_KHR@ specifies that the presentation engine
--     waits for the next vertical blanking period to update the current
--     image. Tearing /cannot/ be observed. An internal single-entry queue
--     is used to hold pending presentation requests. If the queue is full
--     when a new presentation request is received, the new request
--     replaces the existing entry, and any images associated with the
--     prior entry become available for re-use by the application. One
--     request is removed from the queue and processed during each vertical
--     blanking period in which the queue is non-empty.
--
-- -   @VK_PRESENT_MODE_FIFO_KHR@ specifies that the presentation engine
--     waits for the next vertical blanking period to update the current
--     image. Tearing /cannot/ be observed. An internal queue is used to
--     hold pending presentation requests. New requests are appended to the
--     end of the queue, and one request is removed from the beginning of
--     the queue and processed during each vertical blanking period in
--     which the queue is non-empty. This is the only value of
--     @presentMode@ that is /required/ to be supported.
--
-- -   @VK_PRESENT_MODE_FIFO_RELAXED_KHR@ specifies that the presentation
--     engine generally waits for the next vertical blanking period to
--     update the current image. If a vertical blanking period has already
--     passed since the last update of the current image then the
--     presentation engine does not wait for another vertical blanking
--     period for the update, meaning this mode /may/ result in visible
--     tearing in this case. This mode is useful for reducing visual
--     stutter with an application that will mostly present a new image
--     before the next vertical blanking period, but may occasionally be
--     late, and present a new image just after the next vertical blanking
--     period. An internal queue is used to hold pending presentation
--     requests. New requests are appended to the end of the queue, and one
--     request is removed from the beginning of the queue and processed
--     during or after each vertical blanking period in which the queue is
--     non-empty.
--
-- __Note__
--
-- For reference, the mode indicated by @VK_PRESENT_MODE_FIFO_KHR@ is
-- equivalent to the behavior of {wgl|glX|egl}SwapBuffers with a swap
-- interval of 1, while the mode indicated by
-- @VK_PRESENT_MODE_FIFO_RELAXED_KHR@ is equivalent to the behavior of
-- {wgl|glX}SwapBuffers with a swap interval of -1 (from the
-- {WGL|GLX}_EXT_swap_control_tear extensions).
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'vkGetPhysicalDeviceSurfacePresentModesKHR'
newtype VkPresentModeKHR = VkPresentModeKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkPresentModeKHR where
  showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
  showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR = showString "VK_PRESENT_MODE_MAILBOX_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_KHR = showString "VK_PRESENT_MODE_FIFO_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPresentModeKHR 1000111000) = showString "VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
  showsPrec _ (VkPresentModeKHR 1000111001) = showString "VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
  showsPrec p (VkPresentModeKHR x) = showParen (p >= 11) (showString "VkPresentModeKHR " . showsPrec 11 x)

instance Read VkPresentModeKHR where
  readPrec = parens ( choose [ ("VK_PRESENT_MODE_IMMEDIATE_KHR",    pure VK_PRESENT_MODE_IMMEDIATE_KHR)
                             , ("VK_PRESENT_MODE_MAILBOX_KHR",      pure VK_PRESENT_MODE_MAILBOX_KHR)
                             , ("VK_PRESENT_MODE_FIFO_KHR",         pure VK_PRESENT_MODE_FIFO_KHR)
                             , ("VK_PRESENT_MODE_FIFO_RELAXED_KHR", pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR",     pure (VkPresentModeKHR 1000111000))
                             , ("VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR", pure (VkPresentModeKHR 1000111001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPresentModeKHR")
                        v <- step readPrec
                        pure (VkPresentModeKHR v)
                        )
                    )

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_IMMEDIATE_KHR"
pattern VK_PRESENT_MODE_IMMEDIATE_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_MAILBOX_KHR"
pattern VK_PRESENT_MODE_MAILBOX_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_FIFO_KHR"
pattern VK_PRESENT_MODE_FIFO_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3
-- ** VkCompositeAlphaFlagBitsKHR

-- | VkCompositeAlphaFlagBitsKHR - alpha compositing modes supported on a
-- device
--
-- = Description
-- #_description#
--
-- These values are described as follows:
--
-- -   @VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR@: The alpha channel, if it
--     exists, of the images is ignored in the compositing process.
--     Instead, the image is treated as if it has a constant alpha of 1.0.
--
-- -   @VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR@: The alpha channel, if
--     it exists, of the images is respected in the compositing process.
--     The non-alpha channels of the image are expected to already be
--     multiplied by the alpha channel by the application.
--
-- -   @VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR@: The alpha channel, if
--     it exists, of the images is respected in the compositing process.
--     The non-alpha channels of the image are not expected to already be
--     multiplied by the alpha channel by the application; instead, the
--     compositor will multiply the non-alpha channels of the image by the
--     alpha channel during compositing.
--
-- -   @VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR@: The way in which the
--     presentation engine treats the alpha channel in the images is
--     unknown to the Vulkan API. Instead, the application is responsible
--     for setting the composite alpha blending mode using native window
--     system commands. If the application does not set the blending mode
--     using native window system commands, then a platform-specific
--     default will be used.
--
-- = See Also
-- #_see_also#
--
-- 'VkCompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCompositeAlphaFlagBitsKHR where
  showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
  showsPrec p (VkCompositeAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkCompositeAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkCompositeAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR",          pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR",  pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR",         pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCompositeAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkCompositeAlphaFlagBitsKHR v)
                        )
                    )

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000008
-- ** VkSurfaceTransformFlagBitsKHR

-- | VkSurfaceTransformFlagBitsKHR - presentation transforms supported on a
-- device
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'VkSurfaceCapabilitiesKHR', 'VkSurfaceTransformFlagsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSurfaceTransformFlagBitsKHR where
  showsPrec _ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = showString "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = showString "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
  showsPrec p (VkSurfaceTransformFlagBitsKHR x) = showParen (p >= 11) (showString "VkSurfaceTransformFlagBitsKHR " . showsPrec 11 x)

instance Read VkSurfaceTransformFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR",                     pure VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR",                    pure VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR",                   pure VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR",                   pure VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR",            pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR",  pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR",                      pure VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSurfaceTransformFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSurfaceTransformFlagBitsKHR v)
                        )
                    )

-- | @VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR@ specifies that image content is
-- presented without being transformed.
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000001

-- | @VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR@ specifies that image content is
-- rotated 90 degrees clockwise.
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000002

-- | @VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR@ specifies that image content
-- is rotated 180 degrees clockwise.
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000004

-- | @VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR@ specifies that image content
-- is rotated 270 degrees clockwise.
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000008

-- | @VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR@ specifies that image
-- content is mirrored horizontally.
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000010

-- | @VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR@ specifies
-- that image content is mirrored horizontally, then rotated 90 degrees
-- clockwise.
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000020

-- | @VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR@ specifies
-- that image content is mirrored horizontally, then rotated 180 degrees
-- clockwise.
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000040

-- | @VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR@ specifies
-- that image content is mirrored horizontally, then rotated 270 degrees
-- clockwise.
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000080

-- | @VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR@ specifies that the presentation
-- transform is not specified, and is instead determined by
-- platform-specific considerations and mechanisms outside Vulkan.
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000100
-- No documentation found for Nested "VkResult" "VK_ERROR_SURFACE_LOST_KHR"
pattern VK_ERROR_SURFACE_LOST_KHR :: VkResult
pattern VK_ERROR_SURFACE_LOST_KHR = VkResult (-1000000000)
-- No documentation found for Nested "VkResult" "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR"
pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR :: VkResult
pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = VkResult (-1000000001)
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SURFACE_KHR"
pattern VK_OBJECT_TYPE_SURFACE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_SURFACE_KHR = VkObjectType 1000000000
-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern VK_KHR_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SURFACE_SPEC_VERSION = 25
-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern VK_KHR_SURFACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"
-- No documentation found for TopLevel "VK_COLORSPACE_SRGB_NONLINEAR_KHR"
pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR
pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
-- | Dummy data to tag the 'Ptr' with
data VkSurfaceKHR_T
-- | VkSurfaceKHR - Opaque handle to a surface object
--
-- = Description
-- #_description#
--
-- The @VK_KHR_surface@ extension declares the @VkSurfaceKHR@ object, and
-- provides a function for destroying @VkSurfaceKHR@ objects. Separate
-- platform-specific extensions each provide a function for creating a
-- @VkSurfaceKHR@ object for the respective platform. From the
-- application’s perspective this is an opaque handle, just like the
-- handles of other Vulkan objects.
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkCreateDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_KHR_mir_surface.vkCreateMirSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.vkCreateViSurfaceNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR',
-- 'vkDestroySurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetPhysicalDevicePresentRectanglesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT',
-- 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'vkGetPhysicalDeviceSurfaceFormatsKHR',
-- 'vkGetPhysicalDeviceSurfacePresentModesKHR',
-- 'vkGetPhysicalDeviceSurfaceSupportKHR'
type VkSurfaceKHR = Ptr VkSurfaceKHR_T
-- | vkDestroySurfaceKHR - Destroy a VkSurfaceKHR object
--
-- = Parameters
-- #_parameters#
--
-- -   @instance@ is the instance used to create the surface.
--
-- -   @surface@ is the surface to destroy.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see <{html_spec_relative}#memory-allocation Memory Allocation>).
--
-- = Description
-- #_description#
--
-- Destroying a @VkSurfaceKHR@ merely severs the connection between Vulkan
-- and the native surface, and does not imply destroying the native
-- surface, closing a window, or similar behavior.
--
-- == Valid Usage
--
-- -   All @VkSwapchainKHR@ objects created for @surface@ /must/ have been
--     destroyed prior to destroying @surface@
--
-- -   If @VkAllocationCallbacks@ were provided when @surface@ was created,
--     a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @surface@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   If @surface@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @surface@ /must/
--     be a valid @VkSurfaceKHR@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @surface@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @instance@
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance', 'VkSurfaceKHR'
foreign import ccall "vkDestroySurfaceKHR" vkDestroySurfaceKHR :: ("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkGetPhysicalDeviceSurfaceSupportKHR - Query if presentation is
-- supported
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family.
--
-- -   @surface@ is the surface.
--
-- -   @pSupported@ is a pointer to a @VkBool32@, which is set to @VK_TRUE@
--     to indicate support, and @VK_FALSE@ otherwise.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by @vkGetPhysicalDeviceQueueFamilyProperties@ for the given
--     @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @surface@ /must/ be a valid @VkSurfaceKHR@ handle
--
-- -   @pSupported@ /must/ be a valid pointer to a @VkBool32@ value
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@,
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkSurfaceKHR'
foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" vkGetPhysicalDeviceSurfaceSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult
-- | vkGetPhysicalDeviceSurfaceCapabilitiesKHR - Query surface capabilities
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceCapabilities@ is a pointer to an instance of the
--     'VkSurfaceCapabilitiesKHR' structure in which the capabilities are
--     returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @surface@ /must/ be a valid @VkSurfaceKHR@ handle
--
-- -   @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     @VkSurfaceCapabilitiesKHR@ structure
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkSurfaceCapabilitiesKHR', 'VkSurfaceKHR'
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult
-- | vkGetPhysicalDeviceSurfaceFormatsKHR - Query color formats supported by
-- surface
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceFormatCount@ is a pointer to an integer related to the
--     number of format pairs available or queried, as described below.
--
-- -   @pSurfaceFormats@ is either @NULL@ or a pointer to an array of
--     @VkSurfaceFormatKHR@ structures.
--
-- = Description
-- #_description#
--
-- If @pSurfaceFormats@ is @NULL@, then the number of format pairs
-- supported for the given @surface@ is returned in @pSurfaceFormatCount@.
-- The number of format pairs supported will be greater than or equal to 1.
-- Otherwise, @pSurfaceFormatCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSurfaceFormats@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pSurfaceFormats@. If the value of
-- @pSurfaceFormatCount@ is less than the number of format pairs supported,
-- at most @pSurfaceFormatCount@ structures will be written. If
-- @pSurfaceFormatCount@ is smaller than the number of format pairs
-- supported for the given @surface@, @VK_INCOMPLETE@ will be returned
-- instead of @VK_SUCCESS@ to indicate that not all the available values
-- were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @surface@ /must/ be a valid @VkSurfaceKHR@ handle
--
-- -   @pSurfaceFormatCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pSurfaceFormatCount@ is not @0@, and
--     @pSurfaceFormats@ is not @NULL@, @pSurfaceFormats@ /must/ be a valid
--     pointer to an array of @pSurfaceFormatCount@ @VkSurfaceFormatKHR@
--     structures
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkSurfaceFormatKHR', 'VkSurfaceKHR'
foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR" vkGetPhysicalDeviceSurfaceFormatsKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult
-- | vkGetPhysicalDeviceSurfacePresentModesKHR - Query supported presentation
-- modes
--
-- = Parameters
-- #_parameters#
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pPresentModeCount@ is a pointer to an integer related to the number
--     of presentation modes available or queried, as described below.
--
-- -   @pPresentModes@ is either @NULL@ or a pointer to an array of
--     'VkPresentModeKHR' values, indicating the supported presentation
--     modes.
--
-- = Description
-- #_description#
--
-- If @pPresentModes@ is @NULL@, then the number of presentation modes
-- supported for the given @surface@ is returned in @pPresentModeCount@.
-- Otherwise, @pPresentModeCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pPresentModes@ array, and on
-- return the variable is overwritten with the number of values actually
-- written to @pPresentModes@. If the value of @pPresentModeCount@ is less
-- than the number of presentation modes supported, at most
-- @pPresentModeCount@ values will be written. If @pPresentModeCount@ is
-- smaller than the number of presentation modes supported for the given
-- @surface@, @VK_INCOMPLETE@ will be returned instead of @VK_SUCCESS@ to
-- indicate that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @surface@ /must/ be a valid @VkSurfaceKHR@ handle
--
-- -   @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@ 'VkPresentModeKHR' values
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_INCOMPLETE@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'VkPresentModeKHR', 'VkSurfaceKHR'
foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR" vkGetPhysicalDeviceSurfacePresentModesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
-- | VkSurfaceCapabilitiesKHR - Structure describing capabilities of a
-- surface
--
-- = Description
-- #_description#
--
-- __Note__
--
-- Formulas such as min(N, @maxImageCount@) are not correct, since
-- @maxImageCount@ /may/ be zero.
--
-- = See Also
-- #_see_also#
--
-- 'VkCompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceCapabilities2KHR',
-- 'VkSurfaceTransformFlagBitsKHR', 'VkSurfaceTransformFlagsKHR',
-- 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
data VkSurfaceCapabilitiesKHR = VkSurfaceCapabilitiesKHR
  { -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkMinImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkMaxImageCount"
  vkMaxImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkCurrentExtent"
  vkCurrentExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkMinImageExtent"
  vkMinImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkMaxImageExtent"
  vkMaxImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkMaxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkSupportedTransforms"
  vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkCurrentTransform"
  vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkSupportedCompositeAlpha"
  vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "vkSupportedUsageFlags"
  vkSupportedUsageFlags :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkSurfaceCapabilitiesKHR where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek ptr = VkSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMinImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMaxImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMinImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 24) (vkMaxImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 32) (vkMaxImageArrayLayers (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkSupportedTransforms (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 40) (vkCurrentTransform (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkSupportedCompositeAlpha (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 48) (vkSupportedUsageFlags (poked :: VkSurfaceCapabilitiesKHR))
-- | VkSurfaceFormatKHR - Structure describing a supported swapchain
-- format-color space pair
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkColorSpaceKHR', 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceFormat2KHR',
-- 'vkGetPhysicalDeviceSurfaceFormatsKHR'
data VkSurfaceFormatKHR = VkSurfaceFormatKHR
  { -- No documentation found for Nested "VkSurfaceFormatKHR" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkSurfaceFormatKHR" "vkColorSpace"
  vkColorSpace :: VkColorSpaceKHR
  }
  deriving (Eq, Show)

instance Storable VkSurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSurfaceFormatKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFormat (poked :: VkSurfaceFormatKHR))
                *> poke (ptr `plusPtr` 4) (vkColorSpace (poked :: VkSurfaceFormatKHR))
-- | VkCompositeAlphaFlagsKHR - Bitmask of VkCompositeAlphaFlagBitsKHR
--
-- = Description
-- #_description#
--
-- @VkCompositeAlphaFlagsKHR@ is a bitmask type for setting a mask of zero
-- or more 'VkCompositeAlphaFlagBitsKHR'.
--
-- = See Also
-- #_see_also#
--
-- 'VkCompositeAlphaFlagBitsKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'VkSurfaceCapabilitiesKHR'
type VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagBitsKHR
-- | VkSurfaceTransformFlagsKHR - Bitmask of VkSurfaceTransformFlagBitsKHR
--
-- = Description
-- #_description#
--
-- @VkSurfaceTransformFlagsKHR@ is a bitmask type for setting a mask of
-- zero or more 'VkSurfaceTransformFlagBitsKHR'.
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'VkSurfaceCapabilitiesKHR', 'VkSurfaceTransformFlagBitsKHR'
type VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagBitsKHR
