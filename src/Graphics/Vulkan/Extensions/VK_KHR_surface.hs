{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , pattern COLOR_SPACE_SRGB_NONLINEAR_KHR
  , pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
  , pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
  , pattern COLOR_SPACE_DCI_P3_LINEAR_EXT
  , pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT
  , pattern COLOR_SPACE_BT709_LINEAR_EXT
  , pattern COLOR_SPACE_BT709_NONLINEAR_EXT
  , pattern COLOR_SPACE_BT2020_LINEAR_EXT
  , pattern COLOR_SPACE_HDR10_ST2084_EXT
  , pattern COLOR_SPACE_DOLBYVISION_EXT
  , pattern COLOR_SPACE_HDR10_HLG_EXT
  , pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT
  , pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
  , pattern COLOR_SPACE_PASS_THROUGH_EXT
  , pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
  , pattern COLOR_SPACE_DISPLAY_NATIVE_AMD
  , CompositeAlphaFlagBitsKHR
  , pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , CompositeAlphaFlagsKHR
  , PresentModeKHR
  , pattern PRESENT_MODE_IMMEDIATE_KHR
  , pattern PRESENT_MODE_MAILBOX_KHR
  , pattern PRESENT_MODE_FIFO_KHR
  , pattern PRESENT_MODE_FIFO_RELAXED_KHR
  , pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , withCStructSurfaceCapabilitiesKHR
  , fromCStructSurfaceCapabilitiesKHR
  , SurfaceCapabilitiesKHR(..)
  , withCStructSurfaceFormatKHR
  , fromCStructSurfaceFormatKHR
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  , pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , SurfaceTransformFlagsKHR
  , destroySurfaceKHR
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getNumPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getAllPhysicalDeviceSurfaceFormatsKHR
  , getNumPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfacePresentModesKHR
  , getAllPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , pattern KHR_SURFACE_EXTENSION_NAME
  , pattern KHR_SURFACE_SPEC_VERSION
  , pattern ERROR_SURFACE_LOST_KHR
  , pattern ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern OBJECT_TYPE_SURFACE_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_swapchain_colorspace
  ( pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT
  , pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT
  , pattern VK_COLOR_SPACE_BT709_LINEAR_EXT
  , pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT
  , pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DOLBYVISION_EXT
  , pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
  , pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_HDR10_HLG_EXT
  , pattern VK_COLOR_SPACE_HDR10_ST2084_EXT
  , pattern VK_COLOR_SPACE_PASS_THROUGH_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , VkCompositeAlphaFlagBitsKHR(..)
  , VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceKHR
  , vkDestroySurfaceKHR
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , vkGetPhysicalDeviceSurfaceFormatsKHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  , vkGetPhysicalDeviceSurfaceSupportKHR
  , pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  , pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_PRESENT_MODE_FIFO_KHR
  , pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR
  , pattern VK_PRESENT_MODE_IMMEDIATE_KHR
  , pattern VK_PRESENT_MODE_MAILBOX_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , ImageUsageFlags
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern ERROR_SURFACE_LOST_KHR
  , pattern OBJECT_TYPE_SURFACE_KHR
  )


-- | VkColorSpaceKHR - supported color space of the presentation engine
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
type ColorSpaceKHR = VkColorSpaceKHR


{-# complete COLOR_SPACE_SRGB_NONLINEAR_KHR, COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT, COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT, COLOR_SPACE_DCI_P3_LINEAR_EXT, COLOR_SPACE_DCI_P3_NONLINEAR_EXT, COLOR_SPACE_BT709_LINEAR_EXT, COLOR_SPACE_BT709_NONLINEAR_EXT, COLOR_SPACE_BT2020_LINEAR_EXT, COLOR_SPACE_HDR10_ST2084_EXT, COLOR_SPACE_DOLBYVISION_EXT, COLOR_SPACE_HDR10_HLG_EXT, COLOR_SPACE_ADOBERGB_LINEAR_EXT, COLOR_SPACE_ADOBERGB_NONLINEAR_EXT, COLOR_SPACE_PASS_THROUGH_EXT, COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT, COLOR_SPACE_DISPLAY_NATIVE_AMD :: ColorSpaceKHR #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR'
-- specifies support for the sRGB color space.
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT"
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT = VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT"
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT = VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DCI_P3_LINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DCI_P3_LINEAR_EXT = VK_COLOR_SPACE_DCI_P3_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DCI_P3_NONLINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT = VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_BT709_LINEAR_EXT"
pattern COLOR_SPACE_BT709_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_BT709_LINEAR_EXT = VK_COLOR_SPACE_BT709_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_BT709_NONLINEAR_EXT"
pattern COLOR_SPACE_BT709_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_BT709_NONLINEAR_EXT = VK_COLOR_SPACE_BT709_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_BT2020_LINEAR_EXT"
pattern COLOR_SPACE_BT2020_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_BT2020_LINEAR_EXT = VK_COLOR_SPACE_BT2020_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_HDR10_ST2084_EXT"
pattern COLOR_SPACE_HDR10_ST2084_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_HDR10_ST2084_EXT = VK_COLOR_SPACE_HDR10_ST2084_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DOLBYVISION_EXT"
pattern COLOR_SPACE_DOLBYVISION_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DOLBYVISION_EXT = VK_COLOR_SPACE_DOLBYVISION_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_HDR10_HLG_EXT"
pattern COLOR_SPACE_HDR10_HLG_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_HDR10_HLG_EXT = VK_COLOR_SPACE_HDR10_HLG_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_ADOBERGB_LINEAR_EXT"
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT = VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_ADOBERGB_NONLINEAR_EXT"
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT = VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_PASS_THROUGH_EXT"
pattern COLOR_SPACE_PASS_THROUGH_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_PASS_THROUGH_EXT = VK_COLOR_SPACE_PASS_THROUGH_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT"
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DISPLAY_NATIVE_AMD"
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD = VK_COLOR_SPACE_DISPLAY_NATIVE_AMD

-- | VkCompositeAlphaFlagBitsKHR - alpha compositing modes supported on a
-- device
--
-- = Description
--
-- These values are described as follows:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
type CompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR


{-# complete COMPOSITE_ALPHA_OPAQUE_BIT_KHR, COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR, COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR, COMPOSITE_ALPHA_INHERIT_BIT_KHR :: CompositeAlphaFlagBitsKHR #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR':
-- The alpha channel, if it exists, of the images is ignored in the
-- compositing process. Instead, the image is treated as if it has a
-- constant alpha of 1.0.
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR':
-- The alpha channel, if it exists, of the images is respected in the
-- compositing process. The non-alpha channels of the image are expected to
-- already be multiplied by the alpha channel by the application.
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR':
-- The alpha channel, if it exists, of the images is respected in the
-- compositing process. The non-alpha channels of the image are not
-- expected to already be multiplied by the alpha channel by the
-- application; instead, the compositor will multiply the non-alpha
-- channels of the image by the alpha channel during compositing.
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR':
-- The way in which the presentation engine treats the alpha channel in the
-- images is unknown to the Vulkan API. Instead, the application is
-- responsible for setting the composite alpha blending mode using native
-- window system commands. If the application does not set the blending
-- mode using native window system commands, then a platform-specific
-- default will be used.
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR = VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR

-- | VkCompositeAlphaFlagsKHR - Bitmask of VkCompositeAlphaFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagBitsKHR'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagBitsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

-- | VkPresentModeKHR - presentation mode supported for a surface
--
-- = Description
--
-- __Note__
--
-- For reference, the mode indicated by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_FIFO_KHR'
-- is equivalent to the behavior of {wgl|glX|egl}SwapBuffers with a swap
-- interval of 1, while the mode indicated by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_FIFO_RELAXED_KHR'
-- is equivalent to the behavior of {wgl|glX}SwapBuffers with a swap
-- interval of -1 (from the {WGL|GLX}_EXT_swap_control_tear extensions).
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkGetPhysicalDeviceSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR'
type PresentModeKHR = VkPresentModeKHR


{-# complete PRESENT_MODE_IMMEDIATE_KHR, PRESENT_MODE_MAILBOX_KHR, PRESENT_MODE_FIFO_KHR, PRESENT_MODE_FIFO_RELAXED_KHR, PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR, PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: PresentModeKHR #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_IMMEDIATE_KHR'
-- specifies that the presentation engine does not wait for a vertical
-- blanking period to update the current image, meaning this mode /may/
-- result in visible tearing. No internal queuing of presentation requests
-- is needed, as the requests are applied immediately.
pattern PRESENT_MODE_IMMEDIATE_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_IMMEDIATE_KHR = VK_PRESENT_MODE_IMMEDIATE_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_MAILBOX_KHR'
-- specifies that the presentation engine waits for the next vertical
-- blanking period to update the current image. Tearing /cannot/ be
-- observed. An internal single-entry queue is used to hold pending
-- presentation requests. If the queue is full when a new presentation
-- request is received, the new request replaces the existing entry, and
-- any images associated with the prior entry become available for re-use
-- by the application. One request is removed from the queue and processed
-- during each vertical blanking period in which the queue is non-empty.
pattern PRESENT_MODE_MAILBOX_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_MAILBOX_KHR = VK_PRESENT_MODE_MAILBOX_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_FIFO_KHR'
-- specifies that the presentation engine waits for the next vertical
-- blanking period to update the current image. Tearing /cannot/ be
-- observed. An internal queue is used to hold pending presentation
-- requests. New requests are appended to the end of the queue, and one
-- request is removed from the beginning of the queue and processed during
-- each vertical blanking period in which the queue is non-empty. This is
-- the only value of @presentMode@ that is /required/ to be supported.
pattern PRESENT_MODE_FIFO_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_FIFO_KHR = VK_PRESENT_MODE_FIFO_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_FIFO_RELAXED_KHR'
-- specifies that the presentation engine generally waits for the next
-- vertical blanking period to update the current image. If a vertical
-- blanking period has already passed since the last update of the current
-- image then the presentation engine does not wait for another vertical
-- blanking period for the update, meaning this mode /may/ result in
-- visible tearing in this case. This mode is useful for reducing visual
-- stutter with an application that will mostly present a new image before
-- the next vertical blanking period, but may occasionally be late, and
-- present a new image just after the next vertical blanking period. An
-- internal queue is used to hold pending presentation requests. New
-- requests are appended to the end of the queue, and one request is
-- removed from the beginning of the queue and processed during or after
-- each vertical blanking period in which the queue is non-empty.
pattern PRESENT_MODE_FIFO_RELAXED_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_FIFO_RELAXED_KHR = VK_PRESENT_MODE_FIFO_RELAXED_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR


-- | VkSurfaceCapabilitiesKHR - Structure describing capabilities of a
-- surface
--
-- = Description
--
-- __Note__
--
-- Formulas such as min(N, @maxImageCount@) are not correct, since
-- @maxImageCount@ /may/ be zero.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceCapabilities2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
data SurfaceCapabilitiesKHR = SurfaceCapabilitiesKHR
  { -- No documentation found for Nested "SurfaceCapabilitiesKHR" "minImageCount"
  minImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageCount"
  maxImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "currentExtent"
  currentExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "minImageExtent"
  minImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageExtent"
  maxImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageArrayLayers"
  maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedTransforms"
  supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "currentTransform"
  currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedCompositeAlpha"
  supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedUsageFlags"
  supportedUsageFlags :: ImageUsageFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceCapabilitiesKHR' and
-- marshal a 'SurfaceCapabilitiesKHR' into it. The 'VkSurfaceCapabilitiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceCapabilitiesKHR :: SurfaceCapabilitiesKHR -> (VkSurfaceCapabilitiesKHR -> IO a) -> IO a
withCStructSurfaceCapabilitiesKHR marshalled cont = withCStructExtent2D (maxImageExtent (marshalled :: SurfaceCapabilitiesKHR)) (\maxImageExtent'' -> withCStructExtent2D (minImageExtent (marshalled :: SurfaceCapabilitiesKHR)) (\minImageExtent'' -> withCStructExtent2D (currentExtent (marshalled :: SurfaceCapabilitiesKHR)) (\currentExtent'' -> cont (VkSurfaceCapabilitiesKHR (minImageCount (marshalled :: SurfaceCapabilitiesKHR)) (maxImageCount (marshalled :: SurfaceCapabilitiesKHR)) currentExtent'' minImageExtent'' maxImageExtent'' (maxImageArrayLayers (marshalled :: SurfaceCapabilitiesKHR)) (supportedTransforms (marshalled :: SurfaceCapabilitiesKHR)) (currentTransform (marshalled :: SurfaceCapabilitiesKHR)) (supportedCompositeAlpha (marshalled :: SurfaceCapabilitiesKHR)) (supportedUsageFlags (marshalled :: SurfaceCapabilitiesKHR))))))

-- | A function to read a 'VkSurfaceCapabilitiesKHR' and all additional
-- structures in the pointer chain into a 'SurfaceCapabilitiesKHR'.
fromCStructSurfaceCapabilitiesKHR :: VkSurfaceCapabilitiesKHR -> IO SurfaceCapabilitiesKHR
fromCStructSurfaceCapabilitiesKHR c = SurfaceCapabilitiesKHR <$> pure (vkMinImageCount (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkMaxImageCount (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> (fromCStructExtent2D (vkCurrentExtent (c :: VkSurfaceCapabilitiesKHR)))
                                                             <*> (fromCStructExtent2D (vkMinImageExtent (c :: VkSurfaceCapabilitiesKHR)))
                                                             <*> (fromCStructExtent2D (vkMaxImageExtent (c :: VkSurfaceCapabilitiesKHR)))
                                                             <*> pure (vkMaxImageArrayLayers (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkSupportedTransforms (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkCurrentTransform (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkSupportedCompositeAlpha (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkSupportedUsageFlags (c :: VkSurfaceCapabilitiesKHR))

instance Zero SurfaceCapabilitiesKHR where
  zero = SurfaceCapabilitiesKHR zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero



-- | VkSurfaceFormatKHR - Structure describing a supported swapchain
-- format-color space pair
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkColorSpaceKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceFormat2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceFormatsKHR'
data SurfaceFormatKHR = SurfaceFormatKHR
  { -- No documentation found for Nested "SurfaceFormatKHR" "format"
  format :: Format
  , -- No documentation found for Nested "SurfaceFormatKHR" "colorSpace"
  colorSpace :: ColorSpaceKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceFormatKHR' and
-- marshal a 'SurfaceFormatKHR' into it. The 'VkSurfaceFormatKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceFormatKHR :: SurfaceFormatKHR -> (VkSurfaceFormatKHR -> IO a) -> IO a
withCStructSurfaceFormatKHR marshalled cont = cont (VkSurfaceFormatKHR (format (marshalled :: SurfaceFormatKHR)) (colorSpace (marshalled :: SurfaceFormatKHR)))

-- | A function to read a 'VkSurfaceFormatKHR' and all additional
-- structures in the pointer chain into a 'SurfaceFormatKHR'.
fromCStructSurfaceFormatKHR :: VkSurfaceFormatKHR -> IO SurfaceFormatKHR
fromCStructSurfaceFormatKHR c = SurfaceFormatKHR <$> pure (vkFormat (c :: VkSurfaceFormatKHR))
                                                 <*> pure (vkColorSpace (c :: VkSurfaceFormatKHR))

instance Zero SurfaceFormatKHR where
  zero = SurfaceFormatKHR zero
                          zero


-- | VkSurfaceKHR - Opaque handle to a surface object
--
-- = Description
--
-- The @VK_KHR_surface@ extension declares the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object, and
-- provides a function for destroying
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' objects.
-- Separate platform-specific extensions each provide a function for
-- creating a 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
-- object for the respective platform. From the application’s perspective
-- this is an opaque handle, just like the handles of other Vulkan objects.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.vkCreateHeadlessSurfaceEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK',
-- 'Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface.vkCreateImagePipeSurfaceFUCHSIA',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.vkCreateMetalSurfaceEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.vkCreateStreamDescriptorSurfaceGGP',
-- 'Graphics.Vulkan.C.Extensions.VK_NN_vi_surface.vkCreateViSurfaceNN',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkDestroySurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetPhysicalDevicePresentRectanglesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceFormatsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceSupportKHR'
type SurfaceKHR = VkSurfaceKHR

-- | VkSurfaceTransformFlagBitsKHR - presentation transforms supported on a
-- device
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
type SurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR


{-# complete SURFACE_TRANSFORM_IDENTITY_BIT_KHR, SURFACE_TRANSFORM_ROTATE_90_BIT_KHR, SURFACE_TRANSFORM_ROTATE_180_BIT_KHR, SURFACE_TRANSFORM_ROTATE_270_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR, SURFACE_TRANSFORM_INHERIT_BIT_KHR :: SurfaceTransformFlagBitsKHR #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR'
-- specifies that image content is presented without being transformed.
pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR'
-- specifies that image content is rotated 90 degrees clockwise.
pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR'
-- specifies that image content is rotated 180 degrees clockwise.
pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR'
-- specifies that image content is rotated 270 degrees clockwise.
pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR'
-- specifies that image content is mirrored horizontally.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR'
-- specifies that image content is mirrored horizontally, then rotated 90
-- degrees clockwise.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR'
-- specifies that image content is mirrored horizontally, then rotated 180
-- degrees clockwise.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR'
-- specifies that image content is mirrored horizontally, then rotated 270
-- degrees clockwise.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR'
-- specifies that the presentation transform is not specified, and is
-- instead determined by platform-specific considerations and mechanisms
-- outside Vulkan.
pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR = VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR

-- | VkSurfaceTransformFlagsKHR - Bitmask of VkSurfaceTransformFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'
type SurfaceTransformFlagsKHR = SurfaceTransformFlagBitsKHR


-- | vkDestroySurfaceKHR - Destroy a VkSurfaceKHR object
--
-- = Parameters
--
-- -   @instance@ is the instance used to create the surface.
--
-- -   @surface@ is the surface to destroy.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- = Description
--
-- Destroying a 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
-- merely severs the connection between Vulkan and the native surface, and
-- does not imply destroying the native surface, closing a window, or
-- similar behavior.
--
-- == Valid Usage
--
-- -   All 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     objects created for @surface@ /must/ have been destroyed prior to
--     destroying @surface@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @surface@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @surface@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   If @surface@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @surface@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @surface@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @instance@
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
destroySurfaceKHR :: Instance ->  SurfaceKHR ->  Maybe AllocationCallbacks ->  IO ()
destroySurfaceKHR = \(Instance instance' commandTable) -> \surface' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroySurfaceKHR commandTable instance' surface' pAllocator *> (pure ()))


-- | vkGetPhysicalDeviceSurfaceCapabilitiesKHR - Query surface capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceCapabilities@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure in which the capabilities are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getPhysicalDeviceSurfaceCapabilitiesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (SurfaceCapabilitiesKHR)
getPhysicalDeviceSurfaceCapabilitiesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> alloca (\pSurfaceCapabilities' -> vkGetPhysicalDeviceSurfaceCapabilitiesKHR commandTable physicalDevice' surface' pSurfaceCapabilities' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructSurfaceCapabilitiesKHR <=< peek) pSurfaceCapabilities')))


-- | vkGetPhysicalDeviceSurfaceFormatsKHR - Query color formats supported by
-- surface
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceFormatCount@ is a pointer to an integer related to the
--     number of format pairs available or queried, as described below.
--
-- -   @pSurfaceFormats@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR'
--     structures.
--
-- = Description
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
-- supported for the given @surface@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   @pSurfaceFormatCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pSurfaceFormatCount@ is not @0@, and
--     @pSurfaceFormats@ is not @NULL@, @pSurfaceFormats@ /must/ be a valid
--     pointer to an array of @pSurfaceFormatCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR'
--     structures
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getNumPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfaceFormatsKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> alloca (\pSurfaceFormatCount' -> vkGetPhysicalDeviceSurfaceFormatsKHR commandTable physicalDevice' surface' pSurfaceFormatCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pSurfaceFormatCount')))

-- | vkGetPhysicalDeviceSurfaceFormatsKHR - Query color formats supported by
-- surface
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pSurfaceFormatCount@ is a pointer to an integer related to the
--     number of format pairs available or queried, as described below.
--
-- -   @pSurfaceFormats@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR'
--     structures.
--
-- = Description
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
-- supported for the given @surface@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   @pSurfaceFormatCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pSurfaceFormatCount@ is not @0@, and
--     @pSurfaceFormats@ is not @NULL@, @pSurfaceFormats@ /must/ be a valid
--     pointer to an array of @pSurfaceFormatCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR'
--     structures
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector SurfaceFormatKHR)
getPhysicalDeviceSurfaceFormatsKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> \surfaceFormatCount' -> allocaArray (fromIntegral surfaceFormatCount') (\pSurfaceFormats' -> with surfaceFormatCount' (\pSurfaceFormatCount' -> vkGetPhysicalDeviceSurfaceFormatsKHR commandTable physicalDevice' surface' pSurfaceFormatCount' pSurfaceFormats' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructSurfaceFormatKHR <=< peekElemOff p) pSurfaceFormats') =<< (fromIntegral <$> (peek pSurfaceFormatCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceSurfaceFormatsKHR'.
getAllPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector SurfaceFormatKHR)
getAllPhysicalDeviceSurfaceFormatsKHR physicalDevice' surface' =
  snd <$> getNumPhysicalDeviceSurfaceFormatsKHR physicalDevice' surface'
    >>= \num -> snd <$> getPhysicalDeviceSurfaceFormatsKHR physicalDevice' surface' num



-- | vkGetPhysicalDeviceSurfacePresentModesKHR - Query supported presentation
-- modes
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pPresentModeCount@ is a pointer to an integer related to the number
--     of presentation modes available or queried, as described below.
--
-- -   @pPresentModes@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values, indicating the supported presentation modes.
--
-- = Description
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
-- @surface@, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to
-- indicate that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getNumPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfacePresentModesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> alloca (\pPresentModeCount' -> vkGetPhysicalDeviceSurfacePresentModesKHR commandTable physicalDevice' surface' pPresentModeCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPresentModeCount')))

-- | vkGetPhysicalDeviceSurfacePresentModesKHR - Query supported presentation
-- modes
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be associated with
--     the swapchain to be created, as described for
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @surface@ is the surface that will be associated with the swapchain.
--
-- -   @pPresentModeCount@ is a pointer to an integer related to the number
--     of presentation modes available or queried, as described below.
--
-- -   @pPresentModes@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values, indicating the supported presentation modes.
--
-- = Description
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
-- @surface@, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to
-- indicate that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector PresentModeKHR)
getPhysicalDeviceSurfacePresentModesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> \presentModeCount' -> allocaArray (fromIntegral presentModeCount') (\pPresentModes' -> with presentModeCount' (\pPresentModeCount' -> vkGetPhysicalDeviceSurfacePresentModesKHR commandTable physicalDevice' surface' pPresentModeCount' pPresentModes' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM (peekElemOff pPresentModes') =<< (fromIntegral <$> (peek pPresentModeCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceSurfacePresentModesKHR'.
getAllPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector PresentModeKHR)
getAllPhysicalDeviceSurfacePresentModesKHR physicalDevice' surface' =
  snd <$> getNumPhysicalDeviceSurfacePresentModesKHR physicalDevice' surface'
    >>= \num -> snd <$> getPhysicalDeviceSurfacePresentModesKHR physicalDevice' surface' num



-- | vkGetPhysicalDeviceSurfaceSupportKHR - Query if presentation is
-- supported
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family.
--
-- -   @surface@ is the surface.
--
-- -   @pSupported@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.Core.VkBool32', which is set to
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE' to indicate support, and
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE' otherwise.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     for the given @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @surface@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- -   @pSupported@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Core.VkBool32' value
--
-- -   Both of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getPhysicalDeviceSurfaceSupportKHR :: PhysicalDevice ->  Word32 ->  SurfaceKHR ->  IO (Bool)
getPhysicalDeviceSurfaceSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \surface' -> alloca (\pSupported' -> vkGetPhysicalDeviceSurfaceSupportKHR commandTable physicalDevice' queueFamilyIndex' surface' pSupported' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (bool32ToBool <$> (peek pSupported'))))

-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern KHR_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SURFACE_EXTENSION_NAME = VK_KHR_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern KHR_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_SURFACE_SPEC_VERSION = VK_KHR_SURFACE_SPEC_VERSION
