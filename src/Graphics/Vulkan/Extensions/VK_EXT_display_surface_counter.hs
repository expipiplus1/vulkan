{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( withCStructSurfaceCapabilities2EXT
  , fromCStructSurfaceCapabilities2EXT
  , SurfaceCapabilities2EXT(..)
  , SurfaceCounterFlagBitsEXT
  , pattern SURFACE_COUNTER_VBLANK_EXT
  , SurfaceCounterFlagsEXT
  , getPhysicalDeviceSurfaceCapabilities2EXT
  , pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
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
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagBitsEXT(..)
  , vkGetPhysicalDeviceSurfaceCapabilities2EXT
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  , pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  , pattern VK_SURFACE_COUNTER_VBLANK_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  , ImageUsageFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( CompositeAlphaFlagsKHR
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkSurfaceCapabilities2EXT - Structure describing capabilities of a
-- surface
--
-- = Members
--
-- All members of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT'
-- are identical to the corresponding members of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
-- where one exists. The remaining members are:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagsKHR',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT'
data SurfaceCapabilities2EXT = SurfaceCapabilities2EXT
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceCapabilities2EXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "minImageCount"
  minImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageCount"
  maxImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "currentExtent"
  currentExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "minImageExtent"
  minImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageExtent"
  maxImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "maxImageArrayLayers"
  maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedTransforms"
  supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "currentTransform"
  currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedCompositeAlpha"
  supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedUsageFlags"
  supportedUsageFlags :: ImageUsageFlags
  , -- No documentation found for Nested "SurfaceCapabilities2EXT" "supportedSurfaceCounters"
  supportedSurfaceCounters :: SurfaceCounterFlagsEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceCapabilities2EXT' and
-- marshal a 'SurfaceCapabilities2EXT' into it. The 'VkSurfaceCapabilities2EXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceCapabilities2EXT :: SurfaceCapabilities2EXT -> (VkSurfaceCapabilities2EXT -> IO a) -> IO a
withCStructSurfaceCapabilities2EXT marshalled cont = withCStructExtent2D (maxImageExtent (marshalled :: SurfaceCapabilities2EXT)) (\maxImageExtent'' -> withCStructExtent2D (minImageExtent (marshalled :: SurfaceCapabilities2EXT)) (\minImageExtent'' -> withCStructExtent2D (currentExtent (marshalled :: SurfaceCapabilities2EXT)) (\currentExtent'' -> maybeWith withSomeVkStruct (next (marshalled :: SurfaceCapabilities2EXT)) (\pPNext -> cont (VkSurfaceCapabilities2EXT VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT pPNext (minImageCount (marshalled :: SurfaceCapabilities2EXT)) (maxImageCount (marshalled :: SurfaceCapabilities2EXT)) currentExtent'' minImageExtent'' maxImageExtent'' (maxImageArrayLayers (marshalled :: SurfaceCapabilities2EXT)) (supportedTransforms (marshalled :: SurfaceCapabilities2EXT)) (currentTransform (marshalled :: SurfaceCapabilities2EXT)) (supportedCompositeAlpha (marshalled :: SurfaceCapabilities2EXT)) (supportedUsageFlags (marshalled :: SurfaceCapabilities2EXT)) (supportedSurfaceCounters (marshalled :: SurfaceCapabilities2EXT)))))))

-- | A function to read a 'VkSurfaceCapabilities2EXT' and all additional
-- structures in the pointer chain into a 'SurfaceCapabilities2EXT'.
fromCStructSurfaceCapabilities2EXT :: VkSurfaceCapabilities2EXT -> IO SurfaceCapabilities2EXT
fromCStructSurfaceCapabilities2EXT c = SurfaceCapabilities2EXT <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> pure (vkMinImageCount (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkMaxImageCount (c :: VkSurfaceCapabilities2EXT))
                                                               <*> (fromCStructExtent2D (vkCurrentExtent (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> (fromCStructExtent2D (vkMinImageExtent (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> (fromCStructExtent2D (vkMaxImageExtent (c :: VkSurfaceCapabilities2EXT)))
                                                               <*> pure (vkMaxImageArrayLayers (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedTransforms (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkCurrentTransform (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedCompositeAlpha (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedUsageFlags (c :: VkSurfaceCapabilities2EXT))
                                                               <*> pure (vkSupportedSurfaceCounters (c :: VkSurfaceCapabilities2EXT))

instance Zero SurfaceCapabilities2EXT where
  zero = SurfaceCapabilities2EXT Nothing
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero


-- | VkSurfaceCounterFlagBitsEXT - Surface-relative counter types
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkGetSwapchainCounterEXT'
type SurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT


{-# complete SURFACE_COUNTER_VBLANK_EXT :: SurfaceCounterFlagBitsEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VK_SURFACE_COUNTER_VBLANK_EXT'
-- specifies a counter incrementing once every time a vertical blanking
-- period occurs on the display associated with the surface.
pattern SURFACE_COUNTER_VBLANK_EXT :: (a ~ SurfaceCounterFlagBitsEXT) => a
pattern SURFACE_COUNTER_VBLANK_EXT = VK_SURFACE_COUNTER_VBLANK_EXT

-- | VkSurfaceCounterFlagsEXT - Bitmask of VkSurfaceCounterFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.VkSwapchainCounterCreateInfoEXT'
type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT


-- | vkGetPhysicalDeviceSurfaceCapabilities2EXT - Query surface capabilities
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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- with the ability to return extended information by adding extension
-- structures to the @pNext@ chain of its @pSurfaceCapabilities@ parameter.
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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT'
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
getPhysicalDeviceSurfaceCapabilities2EXT :: PhysicalDevice ->  SurfaceKHR ->  IO (SurfaceCapabilities2EXT)
getPhysicalDeviceSurfaceCapabilities2EXT = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> alloca (\pSurfaceCapabilities' -> vkGetPhysicalDeviceSurfaceCapabilities2EXT commandTable physicalDevice' surface' pSurfaceCapabilities' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructSurfaceCapabilities2EXT <=< peek) pSurfaceCapabilities')))

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME"
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION"
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION :: Integral a => a
pattern EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT :: VkStructureType
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT = STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
