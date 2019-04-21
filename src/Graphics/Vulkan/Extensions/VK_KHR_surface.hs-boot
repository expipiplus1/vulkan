{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , CompositeAlphaFlagBitsKHR
  , CompositeAlphaFlagsKHR
  , PresentModeKHR
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR
  , VkCompositeAlphaFlagBitsKHR
  , VkPresentModeKHR
  , VkSurfaceTransformFlagBitsKHR
  , VkSurfaceKHR
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
