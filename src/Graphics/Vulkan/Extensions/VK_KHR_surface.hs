{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , pattern COLOR_SPACE_SRGB_NONLINEAR_KHR
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
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_OBJECT_TYPE_SURFACE_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_SURFACE_KHR
  )


-- | VkColorSpaceKHR - supported color space of the presentation engine
--
-- = Description
--
-- The color components of Non-linear color space swap chain images have
-- had the appropriate transfer function applied. Vulkan requires that all
-- implementations support the sRGB transfer function when using an SRGB
-- pixel format. Other transfer functions, such as SMPTE 170M or SMPTE2084,
-- /must/ not be performed by the implementation, but /can/ be performed by
-- the application shader. This extension defines enums for
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkColorSpaceKHR' that
-- correspond to the following color spaces:
--
-- > +----------+----------+----------+----------+----------+-----------+
-- > | Name     | Red      | Green    | Blue     | White-po | Transfer  |
-- > |          | Primary  | Primary  | Primary  | int      | function  |
-- > +==========+==========+==========+==========+==========+===========+
-- > | DCI-P3   | 0.680,   | 0.265,   | 0.150,   | 0.3127,  | Gamma 2.6 |
-- > |          | 0.320    | 0.690    | 0.060    | 0.3290   |           |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | Display- | 0.680,   | 0.265,   | 0.150,   | 0.3127,  | Display-P |
-- > | P3       | 0.320    | 0.690    | 0.060    | 0.3290   | 3         |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | BT709    | 0.640,   | 0.300,   | 0.150,   | 0.3127,  | SMPTE     |
-- > |          | 0.330    | 0.600    | 0.060    | 0.3290   | 170M      |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | sRGB     | 0.640,   | 0.300,   | 0.150,   | 0.3127,  | sRGB      |
-- > |          | 0.330    | 0.600    | 0.060    | 0.3290   |           |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | extended | 0.640,   | 0.300,   | 0.150,   | 0.3127,  | extended  |
-- > | sRGB     | 0.330    | 0.600    | 0.060    | 0.3290   | sRGB      |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | HDR10_ST | 0.708,   | 0.170,   | 0.131,   | 0.3127,  | ST2084    |
-- > | 2084     | 0.292    | 0.797    | 0.046    | 0.3290   |           |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | DOLBYVIS | 0.708,   | 0.170,   | 0.131,   | 0.3127,  | ST2084    |
-- > | ION      | 0.292    | 0.797    | 0.046    | 0.3290   |           |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | HDR10_HL | 0.708,   | 0.170,   | 0.131,   | 0.3127,  | HLG       |
-- > | G        | 0.292    | 0.797    | 0.046    | 0.3290   |           |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- > | AdobeRGB | 0.640,   | 0.210,   | 0.150,   | 0.3127,  | AdobeRGB  |
-- > |          | 0.330    | 0.710    | 0.060    | 0.3290   |           |
-- > |          |          |          |          | (D65)    |           |
-- > +----------+----------+----------+----------+----------+-----------+
-- >
-- > Color Spaces and Attributes
--
-- For Opto-Electrical Transfer Function (OETF), unless otherwise
-- specified, the values of L and E are defined as:
--
-- L - linear luminance of image \(0 \leq L \leq 1\) for conventional
-- colorimetry
--
-- E - corresponding electrical signal (value stored in memory)
--
-- = See Also
--
-- No cross-references are available
type ColorSpaceKHR = VkColorSpaceKHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR'
-- specifies support for the sRGB color space.
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

-- | VkCompositeAlphaFlagBitsKHR - alpha compositing modes supported on a
-- device
--
-- = Description
--
-- These values are described as follows:
--
-- = See Also
--
-- No cross-references are available
type CompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR


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
-- No cross-references are available
type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

-- | VkPresentModeKHR - presentation mode supported for a surface
--
-- = Description
--
-- The supported
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits' of
-- the presentable images of a swapchain created for a surface /may/ differ
-- depending on the presentation mode, and can be determined as per the
-- table below:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Presentation mode                 | Image usage flags                 |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | KHR_surface.VK_PRESENT_MODE_IMMED | KHR_surface.VkSurfaceCapabilities |
-- > | IATE_KHR'                         | KHR'::@supportedUsageFlags@       |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | KHR_surface.VK_PRESENT_MODE_MAILB | KHR_surface.VkSurfaceCapabilities |
-- > | OX_KHR'                           | KHR'::@supportedUsageFlags@       |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | KHR_surface.VK_PRESENT_MODE_FIFO_ | KHR_surface.VkSurfaceCapabilities |
-- > | KHR'                              | KHR'::@supportedUsageFlags@       |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | KHR_surface.VK_PRESENT_MODE_FIFO_ | KHR_surface.VkSurfaceCapabilities |
-- > | RELAXED_KHR'                      | KHR'::@supportedUsageFlags@       |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | KHR_shared_presentable_image.VK_P | KHR_shared_presentable_image.VkSh |
-- > | RESENT_MODE_SHARED_DEMAND_REFRESH | aredPresentSurfaceCapabilitiesKHR |
-- > | _KHR'                             | '::@sharedPresentSupportedUsageFl |
-- > |                                   | ags@                              |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_ | 'Graphics.Vulkan.C.Extensions.VK_ |
-- > | KHR_shared_presentable_image.VK_P | KHR_shared_presentable_image.VkSh |
-- > | RESENT_MODE_SHARED_CONTINUOUS_REF | aredPresentSurfaceCapabilitiesKHR |
-- > | RESH_KHR'                         | '::@sharedPresentSupportedUsageFl |
-- > |                                   | ags@                              |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Presentable image usage queries
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
-- No cross-references are available
type PresentModeKHR = VkPresentModeKHR


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


-- | VkSurfaceCapabilitiesKHR - Structure describing capabilities of a
-- surface
--
-- = Description
--
-- __Note__
--
-- Supported usage flags of a presentable image when using
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
-- or
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
-- presentation mode are provided by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VkSharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@.
--
-- __Note__
--
-- Formulas such as min(N, @maxImageCount@) are not correct, since
-- @maxImageCount@ /may/ be zero.
--
-- Unresolved directive in VkSurfaceCapabilitiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkSurfaceCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- = Description
--
-- Unresolved directive in VkSurfaceFormatKHR.txt -
-- include::{generated}\/validity\/structs\/VkSurfaceFormatKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- No cross-references are available
type SurfaceKHR = VkSurfaceKHR

-- | VkSurfaceTransformFlagBitsKHR - presentation transforms supported on a
-- device
--
-- = See Also
--
-- No cross-references are available
type SurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR


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
-- No cross-references are available
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
-- Unresolved directive in vkDestroySurfaceKHR.txt -
-- include::{generated}\/validity\/protos\/vkDestroySurfaceKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- = Description
--
-- Unresolved directive in vkGetPhysicalDeviceSurfaceCapabilitiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkGetPhysicalDeviceSurfaceFormatsKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceFormatsKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkGetPhysicalDeviceSurfaceFormatsKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceFormatsKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkGetPhysicalDeviceSurfacePresentModesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfacePresentModesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkGetPhysicalDeviceSurfacePresentModesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfacePresentModesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- Unresolved directive in vkGetPhysicalDeviceSurfaceSupportKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSurfaceSupportKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceSurfaceSupportKHR :: PhysicalDevice ->  Word32 ->  SurfaceKHR ->  IO (Bool)
getPhysicalDeviceSurfaceSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \surface' -> alloca (\pSupported' -> vkGetPhysicalDeviceSurfaceSupportKHR commandTable physicalDevice' queueFamilyIndex' surface' pSupported' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (bool32ToBool <$> (peek pSupported'))))
